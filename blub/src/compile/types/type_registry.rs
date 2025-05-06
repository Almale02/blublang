use std::{collections::HashMap, fmt::Display};

use enum_as_inner::EnumAsInner;

use crate::{blub_compile_error, blub_ice, compile::parser::ast::AstType};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct TypeHandle(pub u64);

pub struct TypeRegistry {
    pub curr_id: u64,
    type_handle_to_info: HashMap<TypeHandle, TypeInfo>,
    type_info_to_handle: HashMap<TypeInfo, TypeHandle>,
    pub struct_name_to_handle: HashMap<String, TypeHandle>,
    pub fn_name_to_handle: HashMap<String, TypeHandle>,
}
impl Default for TypeRegistry {
    fn default() -> Self {
        Self {
            curr_id: 1,
            type_handle_to_info: Default::default(),
            type_info_to_handle: Default::default(),
            struct_name_to_handle: Default::default(),
            fn_name_to_handle: Default::default(),
        }
    }
}
impl TypeRegistry {
    pub fn info_to_string(&self, info: TypeInfo) -> String {
        info.to_string(self)
    }
    pub fn type_to_string(&self, handle: TypeHandle) -> String {
        self.get_type_info(handle).to_string(self)
    }
    pub fn contains_type(&self, info: &TypeInfo) -> bool {
        self.type_info_to_handle.contains_key(info)
    }
    pub fn get_type_info(&self, handle: TypeHandle) -> TypeInfo {
        self.type_handle_to_info
            .get(&handle)
            .unwrap_or_else(|| blub_ice!("didnt find type info for handle {:?}", handle))
            .clone()
    }
    pub fn get_type_handle(&self, info: &TypeInfo) -> TypeHandle {
        *self.type_info_to_handle.get(info).unwrap()
    }
    pub fn get_or_add_type(&mut self, info: TypeInfo) -> TypeHandle {
        if let Some(handle) = self.type_info_to_handle.get(&info) {
            return *handle;
        }
        self.add_type(info)
    }
    pub fn add_type(&mut self, info: TypeInfo) -> TypeHandle {
        if self.contains_type(&info) {
            blub_compile_error!(
                "tried to re register type with info: {}",
                info.to_string(self)
            );
        }
        let type_id = self.curr_id;
        let type_handle = TypeHandle(type_id);
        self.curr_id += 1;
        self.type_handle_to_info.insert(type_handle, info.clone());
        self.type_info_to_handle.insert(info.clone(), type_handle);

        if let TypeInfo::Struct { name, .. } = info {
            if self
                .struct_name_to_handle
                .insert(name.clone(), type_handle)
                .is_some()
            {
                blub_ice!("redefintion of struct {}", name);
            };
        }

        type_handle
    }
    pub fn define_type(&mut self, handle: TypeHandle, info: TypeInfo) {
        self.type_handle_to_info.insert(handle, info.clone());
        self.type_info_to_handle.insert(info, handle);
    }
    pub fn add_primitives(&mut self) {
        self.add_type(TypeInfo::Unit);
        self.add_type(TypeInfo::Str);
        self.add_type(TypeInfo::Bool);
        self.add_type(TypeInfo::StaticType);
        self.add_type(TypeInfo::Number(NumberTypes::I8));
        self.add_type(TypeInfo::Number(NumberTypes::I16));
        self.add_type(TypeInfo::Number(NumberTypes::I32));
        self.add_type(TypeInfo::Number(NumberTypes::I64));
        self.add_type(TypeInfo::Number(NumberTypes::I128));
        self.add_type(TypeInfo::Number(NumberTypes::U8));
        self.add_type(TypeInfo::Number(NumberTypes::U16));
        self.add_type(TypeInfo::Number(NumberTypes::U32));
        self.add_type(TypeInfo::Number(NumberTypes::U64));
        self.add_type(TypeInfo::Number(NumberTypes::U128));
        self.add_type(TypeInfo::Number(NumberTypes::Usize));
        self.add_type(TypeInfo::Number(NumberTypes::F32));
        self.add_type(TypeInfo::Number(NumberTypes::F64));
    }
    pub fn is_type_defined(&self, handle: TypeHandle) -> bool {
        self.type_handle_to_info.contains_key(&handle)
    }
    #[rustfmt::skip]
    pub fn resolve_type_handle_from_ast_type(&mut self, ast_type: AstType) -> TypeHandleResolveRes {
        match ast_type {
            AstType::Symbol(symbol) => match symbol.as_str() {
                "i8" => TypeHandleResolveRes::Symbol { handle: self.get_type_handle(&TypeInfo::Number(NumberTypes::I8)) },
                "i16" => TypeHandleResolveRes::Symbol { handle: self.get_type_handle(&TypeInfo::Number(NumberTypes::I16)) },
                "i32" => TypeHandleResolveRes::Symbol { handle: self.get_type_handle(&TypeInfo::Number(NumberTypes::I32)) },
                "i64" => TypeHandleResolveRes::Symbol { handle: self.get_type_handle(&TypeInfo::Number(NumberTypes::I64)) },
                "i128" => TypeHandleResolveRes::Symbol { handle: self.get_type_handle(&TypeInfo::Number(NumberTypes::I128)) },
                "u8" => TypeHandleResolveRes::Symbol { handle: self.get_type_handle(&TypeInfo::Number(NumberTypes::U8)) },
                "u16" => TypeHandleResolveRes::Symbol { handle: self.get_type_handle(&TypeInfo::Number(NumberTypes::U16)) },
                "u32" => TypeHandleResolveRes::Symbol { handle: self.get_type_handle(&TypeInfo::Number(NumberTypes::U32)) },
                "u64" => TypeHandleResolveRes::Symbol { handle: self.get_type_handle(&TypeInfo::Number(NumberTypes::U64)) },
                "u128" => TypeHandleResolveRes::Symbol { handle: self.get_type_handle(&TypeInfo::Number(NumberTypes::U128)) },
                "usize" => TypeHandleResolveRes::Symbol { handle: self.get_type_handle(&TypeInfo::Number(NumberTypes::Usize)) },
                "f32" => TypeHandleResolveRes::Symbol { handle: self.get_type_handle(&TypeInfo::Number(NumberTypes::F32)) },
                "f64" => TypeHandleResolveRes::Symbol { handle: self.get_type_handle(&TypeInfo::Number(NumberTypes::F64)) },
                "str" => TypeHandleResolveRes::Symbol { handle: self.get_type_handle(&TypeInfo::Str) },
                "bool" => TypeHandleResolveRes::Symbol { handle: self.get_type_handle(&TypeInfo::Bool) },
                "Unit" => TypeHandleResolveRes::Symbol { handle: self.get_type_handle(&TypeInfo::Unit) },
                _ => {
                    if let Some(struct_handle) = self.struct_name_to_handle.get(&symbol) {
                        TypeHandleResolveRes::Struct { handle: *struct_handle, generics: Vec::new() }
                    } else {
                        let new_handle = TypeHandle(self.curr_id);
                        self.curr_id += 1;
                        self.struct_name_to_handle.insert(symbol, new_handle);

                        TypeHandleResolveRes::Struct { handle: new_handle, generics: Vec::new() }
                    }

                },
            },
            AstType::Array(item_ast_type) => {
                let item_resolve = self.resolve_type_handle_from_ast_type(*item_ast_type);

                let info = TypeInfo::Array { handle: item_resolve.get_handle() };

                let array_handle = if !self.contains_type(&info) {
                    self.add_type(info)
                } else {
                    self.get_type_handle(&info)
                };

                TypeHandleResolveRes::Array { handle: array_handle, item_resolve: Box::new(item_resolve) }
            },
            AstType::Pointer { is_mut, pointee } => {
                let pointee_resolve = self.resolve_type_handle_from_ast_type(*pointee);
                let info = TypeInfo::Pointer { is_mut, pointee: pointee_resolve.get_handle() };

                let pointer_handle = if !self.contains_type(&info) {
                    self.add_type(info)
                } else {
                    self.get_type_handle(&info)
                };
                TypeHandleResolveRes::Pointer { handle: pointer_handle, pointee_resolve: Box::new(pointee_resolve) }
            },
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, EnumAsInner)]
pub enum TypeInfo {
    Struct {
        name: String,
        fields: Vec<StructField>,
    },
    Fn {
        return_type: TypeHandle,
        args: Vec<TypeHandle>,
    },
    Number(NumberTypes),
    Array {
        handle: TypeHandle,
    },
    Pointer {
        is_mut: bool,
        pointee: TypeHandle,
    },
    StaticType,
    Str,
    Unit,
    Bool,
}
impl TypeInfo {
    pub fn to_id(&self) -> TypeInfoId {
        match self {
            TypeInfo::Struct { .. } => TypeInfoId::Struct,
            TypeInfo::Fn { .. } => TypeInfoId::Fn,
            TypeInfo::Number(_) => TypeInfoId::Number,
            TypeInfo::Array { .. } => TypeInfoId::Array,
            TypeInfo::Pointer { .. } => TypeInfoId::Pointer,
            TypeInfo::StaticType => TypeInfoId::StaticType,
            TypeInfo::Str => TypeInfoId::Str,
            TypeInfo::Unit => TypeInfoId::Unit,
            TypeInfo::Bool => TypeInfoId::Bool,
        }
    }
    pub fn to_string(&self, type_reg: &TypeRegistry) -> String {
        match self {
            TypeInfo::Struct { name, .. } => name.clone(),
            TypeInfo::Fn { return_type, args } => {
                let mut string = String::new();
                string.push_str("fn (");
                args.iter().copied().for_each(|arg| {
                    string.push_str(
                        format!("{}, ", type_reg.get_type_info(arg).to_string(type_reg)).as_str(),
                    );
                });
                format!(
                    "{}): {}",
                    string,
                    type_reg.get_type_info(*return_type).to_string(type_reg)
                )
            }
            TypeInfo::Array { handle } => {
                format!("[]{}", type_reg.get_type_info(*handle).to_string(type_reg))
            }
            TypeInfo::Pointer { is_mut, pointee } => {
                if *is_mut {
                    format!(
                        "*mut {}",
                        type_reg.get_type_info(*pointee).to_string(type_reg)
                    )
                } else {
                    format!("*{}", type_reg.get_type_info(*pointee).to_string(type_reg))
                }
            }
            TypeInfo::Number(number_types) => format!("{}", number_types),
            TypeInfo::StaticType => "StaticType".into(),
            TypeInfo::Str => "str".into(),
            TypeInfo::Unit => "Unit".into(),
            TypeInfo::Bool => "bool".into(),
        }
    }
}
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum TypeInfoId {
    Struct,
    Fn,
    Number,
    Array,
    Pointer,
    StaticType,
    Str,
    Unit,
    Bool,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StructField {
    pub name: String,
    pub type_handle: TypeHandle,
    pub is_pub: bool,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NumberTypes {
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    F32,
    F64,
}
impl Display for NumberTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            NumberTypes::I8 => "i8",
            NumberTypes::I16 => "i16",
            NumberTypes::I32 => "i32",
            NumberTypes::I64 => "i64",
            NumberTypes::I128 => "i128",
            NumberTypes::U8 => "u8",
            NumberTypes::U16 => "u16",
            NumberTypes::U32 => "u32",
            NumberTypes::U64 => "u64",
            NumberTypes::U128 => "u128",
            NumberTypes::Usize => "usize",
            NumberTypes::F32 => "f32",
            NumberTypes::F64 => "f64",
        })
    }
}
#[derive(Clone, Debug, EnumAsInner)]
pub enum TypeHandleResolveRes {
    Symbol {
        handle: TypeHandle,
    },
    Pointer {
        handle: TypeHandle,
        pointee_resolve: Box<TypeHandleResolveRes>,
    },
    Array {
        handle: TypeHandle,
        item_resolve: Box<TypeHandleResolveRes>,
    },
    Struct {
        handle: TypeHandle,
        generics: Vec<TypeHandleResolveRes>,
    },
}
impl TypeHandleResolveRes {
    pub fn get_handle(&self) -> TypeHandle {
        match self {
            TypeHandleResolveRes::Symbol { handle } => *handle,
            TypeHandleResolveRes::Pointer { handle, .. } => *handle,
            TypeHandleResolveRes::Array { handle, .. } => *handle,
            TypeHandleResolveRes::Struct { handle, .. } => *handle,
        }
    }
}
