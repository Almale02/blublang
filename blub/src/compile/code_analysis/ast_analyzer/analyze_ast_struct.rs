use crate::{
    blub_compile_error,
    compile::{
        code_analysis::code_analyzer::{AstAnalyzer, CodeAnalyzerData},
        parser::ast::Stmt,
        types::type_registry::{
            StructField, TypeHandleResolveRes,
            TypeInfo::{self},
            TypeRegistry,
        },
    },
};

use super::get_decls::GetDecl;

pub struct AnalyzeAstStruct;

impl AstAnalyzer for AnalyzeAstStruct {
    fn analize(
        &mut self,
        code_analyzer: &crate::compile::code_analysis::code_analyzer::CodeAnalyzer,
    ) {
        let decls = code_analyzer.data.get::<GetDecl>();
        define_structs(decls.structs.clone(), code_analyzer.data);
    }
}
fn define_structs(struct_stmts: Vec<Stmt>, data: &CodeAnalyzerData) {
    let decls = data.get::<GetDecl>();
    let type_reg = data.get_mut::<TypeRegistry>();
    let structs = struct_stmts
        .iter()
        .map(|x| x.clone().into_struct_decl().unwrap())
        .collect::<Vec<_>>();
    for (i, (_is_extern, _is_pub, name, _ast_fields)) in structs.iter().enumerate() {
        if let Some(handle) = type_reg.struct_name_to_handle.get(name) {
            if type_reg.is_type_defined(*handle) {
                continue;
            }
        }
        let types_to_define = define_struct_type(struct_stmts[i].clone(), data);
        for t in &types_to_define {
            if !decls.struct_map.contains_key(t) {
                blub_compile_error!("expected struct {} to be defined", t);
            }
        }
        define_structs(
            types_to_define
                .iter()
                .map(|name| decls.structs[*decls.struct_map.get(name).unwrap()].clone())
                .collect::<Vec<_>>(),
            data,
        );
    }
}
fn define_struct_type(stmt: Stmt, data: &CodeAnalyzerData) -> Vec<String> {
    let type_reg = data.get_mut::<TypeRegistry>();
    let mut types_to_be_defined = Vec::new();
    let (_is_extern, _is_pub, name, fields) = stmt.into_struct_decl().unwrap();

    let type_struct_fields = fields
        .iter()
        .map(|ast_field| {
            let field_resolve =
                type_reg.resolve_type_handle_from_ast_type(ast_field.field_type.clone());
            if let TypeHandleResolveRes::Struct {
                handle,
                generics: _,
            } = field_resolve.clone()
            {
                if !type_reg.is_type_defined(handle) {
                    types_to_be_defined.push(ast_field.field_type.clone().into_symbol().unwrap());
                }
            };
            StructField {
                name: ast_field.name.clone(),
                type_handle: field_resolve.get_handle(),
                is_pub: ast_field.is_pub,
            }
        })
        .collect::<Vec<_>>();
    let struct_info = TypeInfo::Struct {
        name: name.clone(),
        fields: type_struct_fields,
    };
    if let Some(handle) = type_reg.struct_name_to_handle.get(&name) {
        type_reg.define_type(*handle, struct_info);
    } else {
        type_reg.add_type(struct_info);
    }
    types_to_be_defined
}
