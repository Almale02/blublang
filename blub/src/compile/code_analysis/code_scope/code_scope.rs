use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use petgraph::{Direction, prelude::DiGraphMap};

use crate::{
    blub_compile_error, blub_ice,
    compile::{
        code_analysis::code_analyzer::CodeAnalyzerData,
        parser::ast::{ArrayInitExprKind, Expr, Stmt},
        types::type_registry::{NumberTypes, TypeHandle, TypeInfo, TypeRegistry},
    },
};

use super::ast_analysis::{AnalysisExpr, AnalysisStmt, AnalysisStructField, ArrayInitAnalysisKind};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct CodeExprHandle(pub u64);

impl Display for CodeExprHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.to_string().as_str())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct CodeSubExprHandle(pub u64);

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AnalysisStmtHandle {
    pub scope: CodeScopeHandle,
    pub idx: usize,
}
impl AnalysisStmtHandle {
    pub fn is_valid(&self) -> bool {
        self.scope.0 != 0
    }
}

pub struct CodeScope {
    pub handle: CodeScopeHandle,
    pub parent: Option<CodeScopeHandle>,
    pub in_fn: Option<String>,
    //
    pub stmts: Vec<AnalysisStmt>,
    pub var_name_to_expr: HashMap<String, CodeExprHandle>,
}
#[derive(Clone, Copy, Debug)]
pub enum TypeOrInferHandle {
    Type(TypeHandle),
    Infer(InferTypeHandle),
}
impl CodeScope {
    pub fn new(
        handle: CodeScopeHandle,
        parent: Option<CodeScopeHandle>,
        in_fn: Option<String>,
    ) -> Self {
        Self {
            handle,
            parent,
            in_fn,
            stmts: Vec::new(),
            var_name_to_expr: HashMap::new(),
        }
    }

    pub fn get_var_expr_handle(
        &self,
        name: &String,
        parser: &CodeScopeParser,
    ) -> Option<(CodeExprHandle, CodeScopeHandle)> {
        if let Some(expr) = self.var_name_to_expr.get(name) {
            return Some((*expr, self.handle));
        }

        let mut current_parent_handle = self.parent;
        while let Some(parent_handle) = current_parent_handle {
            let parent_scope = parser.get_scope_ref(parent_handle);
            if let Some(expr) = parent_scope.var_name_to_expr.get(name) {
                return Some((*expr, parent_handle));
            }
            current_parent_handle = parent_scope.parent;
        }

        None
    }
    pub fn get_var_type(
        &self,
        name: &String,
        parser: &CodeScopeParser,
    ) -> Option<(TypeHandle, CodeScopeHandle)> {
        if let Some((expr, scope)) = self.get_var_expr_handle(name, parser) {
            return Some((parser.expr_to_type(expr), scope));
        }
        None
    }
    pub fn parse_code_block(&mut self, block: Vec<Stmt>, data: &CodeAnalyzerData) {
        block
            .iter()
            .for_each(|x| self.parse_ast_stmt(x.clone(), data));
    }
    #[allow(clippy::collapsible_match)]
    pub fn parse_ast_expr<'a>(
        &'a mut self,
        expr: Expr,
        data: &'a CodeAnalyzerData,
    ) -> CodeExprHandle {
        let type_reg = data.get_mut::<TypeRegistry>();
        let parser = data.get_mut::<CodeScopeParser>();
        match expr {
            Expr::Number(number) => {
                let expr_handle = parser.new_expr(self.handle);
                let analysis_expr = AnalysisExpr::Number {
                    num: number.clone(),
                    expr: expr_handle,
                };
                parser.add_type_infer(expr_handle, Box::new(|type_handle, _reg| type_handle));
                parser.add_expr_infer_validator(
                    expr_handle,
                    Box::new(|info, reg| {
                        if info.is_number() {
                        } else {
                            blub_compile_error!(
                                "mismatched infer type: expected number but got {}",
                                reg.info_to_string(info)
                            )
                        }
                    }),
                );

                parser.expr_to_analysis.insert(expr_handle, analysis_expr);
                parser.add_expr_debug_name(expr_handle, format!("{}", number));

                expr_handle
            }
            Expr::String(string) => {
                let expr_handle = parser.new_expr(self.handle);
                let analysis_expr = AnalysisExpr::String {
                    string: string.clone(),
                    expr: expr_handle,
                };
                let type_handle = type_reg.get_type_handle(&TypeInfo::Str);

                parser.expr_type_map.insert(expr_handle, type_handle);
                parser.expr_to_analysis.insert(expr_handle, analysis_expr);
                parser.add_expr_debug_name(expr_handle, format!(r#""{}""#, string));

                expr_handle
            }
            Expr::Ident(base_name) => {
                let expr_handle = parser.new_expr(self.handle);
                if let Some((var_expr_handle, _)) = self.get_var_expr_handle(&base_name, parser) {
                    if let Some(type_handle) = parser
                        .expr_to_static_type
                        .get(&self.get_var_expr_handle(&base_name, parser).unwrap().0)
                    {
                        parser.expr_to_static_type.insert(expr_handle, *type_handle);
                    }
                    parser.copy_type_or_infer_handle(
                        var_expr_handle,
                        expr_handle,
                        Box::new(|handle, _| handle),
                        data,
                    );
                    parser.add_expr_debug_name(
                        var_expr_handle,
                        format!("ident variable {}", base_name),
                    );
                } else if let Some(fn_type) = type_reg.fn_name_to_handle.get(&base_name) {
                    parser.expr_type_map.insert(expr_handle, *fn_type);
                } else if let Some(struct_type) = type_reg.struct_name_to_handle.get(&base_name) {
                    parser.expr_to_static_type.insert(expr_handle, *struct_type);
                    parser
                        .expr_type_map
                        .insert(expr_handle, type_reg.get_type_handle(&TypeInfo::StaticType));
                } else {
                    blub_compile_error!(
                        "expected variable, function, or struct '{}' to be defined",
                        base_name
                    );
                }
                parser.expr_to_analysis.insert(
                    expr_handle,
                    AnalysisExpr::Ident {
                        name: base_name,
                        expr: expr_handle,
                    },
                );

                expr_handle
            }
            Expr::Access { left, ident } => {
                let left_expr_handle = self.parse_ast_expr(*left, data);
                let left_type_handle = parser.expr_to_type(left_expr_handle);
                let left_type_info = type_reg.get_type_info(left_type_handle);

                let expr_handle = parser.new_expr(self.handle);

                match left_type_info {
                    TypeInfo::Struct {
                        name: struct_name,
                        fields,
                    } => {
                        let mut field_type = None;
                        for field in fields {
                            if field.name == ident {
                                field_type = Some(field.type_handle);
                            }
                        }
                        if let Some(field_type) = field_type {
                            parser.expr_type_map.insert(expr_handle, field_type);
                            parser.expr_to_analysis.insert(
                                expr_handle,
                                AnalysisExpr::Access {
                                    base: left_expr_handle,
                                    ident,
                                    expr: expr_handle,
                                },
                            );
                        } else {
                            blub_compile_error!(
                                "could not find field {} on struct {}",
                                &ident,
                                struct_name
                            );
                        }
                    }
                    _ => blub_compile_error!(
                        "{} doesnt have fields to access",
                        type_reg.type_to_string(left_type_handle)
                    ),
                }

                expr_handle
            }
            Expr::Call {
                base,
                args: call_args,
            } => {
                let base_expr_handle = self.parse_ast_expr(*base, data);
                let base_type = parser.expr_to_type(base_expr_handle);
                let type_info = type_reg.get_type_info(base_type);

                if let TypeInfo::Fn {
                    return_type,
                    args: fn_args,
                } = type_info.clone()
                {
                    if call_args.len() != fn_args.len() {
                        blub_compile_error!(
                            "function {} expected {} args but got {}",
                            type_info.to_string(type_reg),
                            fn_args.len(),
                            call_args.len()
                        );
                    }
                    let mut args = Vec::new();
                    for (i, arg) in call_args.iter().enumerate() {
                        let type_reg = data.get_mut::<TypeRegistry>();
                        let arg_expr_handle = self.parse_ast_expr(arg.clone(), data);
                        let expected_type = match type_info.clone() {
                            TypeInfo::Fn {
                                return_type: _,
                                args,
                            } => args[i],
                            _ => unreachable!(),
                        };
                        parser
                            .get_expected_expr_type(arg_expr_handle, expected_type, data)
                            .unwrap_or_else(|x| {
                                blub_compile_error!(
                                    "expected type {}, but found type {} at fn call {} at arg {}",
                                    type_reg.type_to_string(expected_type),
                                    type_reg.type_to_string(x),
                                    type_reg.info_to_string(type_info.clone()),
                                    i
                                )
                            });

                        args.push(arg_expr_handle);
                    }
                    let expr_handle = parser.new_expr(self.handle);
                    parser.expr_type_map.insert(expr_handle, return_type);
                    parser.expr_to_analysis.insert(
                        expr_handle,
                        AnalysisExpr::Call {
                            base: base_expr_handle,
                            args,
                            expr: expr_handle,
                        },
                    );
                    return expr_handle;
                }

                blub_compile_error!("only functions can be called");
            }
            #[allow(unused_assignments, unused_variables)]
            Expr::Arithmetic { left, op, right } => {
                let left_expr_handle = self.parse_ast_expr(*left, data);
                let right_expr_handle = self.parse_ast_expr(*right, data);

                let mut left_type_handle = None;
                let mut right_type_handle = None;

                let mut got_type = true;

                match parser.get_type_or_infer(left_expr_handle) {
                    TypeOrInferHandle::Type(type_handle) => {
                        left_type_handle = Some(type_handle);
                        right_type_handle = Some(
                            parser
                                .get_expected_expr_type(right_expr_handle, type_handle, data)
                                .unwrap_or_else(|found| {
                                    blub_compile_error!(
                                        "expected right side to be {}, but was {}",
                                        type_reg.type_to_string(type_handle),
                                        type_reg.type_to_string(found)
                                    )
                                }),
                        );
                    }
                    TypeOrInferHandle::Infer(left_infer_type_handle) => {
                        match parser.get_type_or_infer(right_expr_handle) {
                            TypeOrInferHandle::Type(type_handle) => {
                                right_type_handle = Some(type_handle);
                                left_type_handle = Some(
                                    parser
                                        .get_expected_expr_type(left_expr_handle, type_handle, data)
                                        .unwrap_or_else(|found| {
                                            blub_compile_error!(
                                                "expected left side to be {}, but was {}",
                                                type_reg.type_to_string(type_handle),
                                                type_reg.type_to_string(found)
                                            )
                                        }),
                                );
                            }
                            TypeOrInferHandle::Infer(right_infer_type_handle) => {
                                got_type = false;
                            }
                        }
                    }
                }
                let expr_handle = parser.new_expr(self.handle);
                parser.add_expr_debug_name(expr_handle, format!("binary {}", op));

                if !got_type {
                    let left_infer_handle = *parser.expr_infer_map.get(&left_expr_handle).unwrap();
                    let right_infer_handle = parser.expr_infer_map.get(&right_expr_handle).unwrap();

                    parser.merge_infer_handles(left_infer_handle, *right_infer_handle);

                    parser.expr_graph_add_edge(expr_handle, left_expr_handle);
                    parser.expr_graph_add_edge(expr_handle, right_expr_handle);

                    parser
                        .expr_infer_applyer_map
                        .insert(expr_handle, Box::new(|handle, _| handle));
                    parser.expr_infer_map.insert(expr_handle, left_infer_handle);
                    parser
                        .infer_expr_map
                        .get_mut(&left_infer_handle)
                        .unwrap()
                        .insert(expr_handle);
                } else {
                    parser
                        .expr_type_map
                        .insert(expr_handle, parser.expr_to_type(left_expr_handle));
                }

                parser.expr_to_analysis.insert(
                    expr_handle,
                    AnalysisExpr::Arithmetic {
                        lhs: left_expr_handle,
                        op,
                        rhs: right_expr_handle,
                        expr: expr_handle,
                    },
                );

                expr_handle
            }
            #[allow(unused_assignments, unused_variables)]
            Expr::Comparison { left, op, right } => {
                let left_expr_handle = self.parse_ast_expr(*left, data);
                let right_expr_handle = self.parse_ast_expr(*right, data);

                let mut left_type_handle = None;
                let mut right_type_handle = None;

                match parser.get_type_or_infer(left_expr_handle) {
                    TypeOrInferHandle::Type(type_handle) => {
                        left_type_handle = Some(type_handle);
                        right_type_handle = Some(
                            parser
                                .get_expected_expr_type(right_expr_handle, type_handle, data)
                                .unwrap_or_else(|found| {
                                    blub_compile_error!(
                                        "expected right side to be {}, but was {}",
                                        type_reg.type_to_string(type_handle),
                                        type_reg.type_to_string(found)
                                    )
                                }),
                        );
                    }
                    TypeOrInferHandle::Infer(left_infer_type_handle) => {
                        match parser.get_type_or_infer(right_expr_handle) {
                            TypeOrInferHandle::Type(type_handle) => {
                                right_type_handle = Some(type_handle);
                                left_type_handle = Some(
                                    parser
                                        .get_expected_expr_type(left_expr_handle, type_handle, data)
                                        .unwrap_or_else(|found| {
                                            blub_compile_error!(
                                                "expected left side to be {}, but was {}",
                                                type_reg.type_to_string(type_handle),
                                                type_reg.type_to_string(found)
                                            )
                                        }),
                                );
                            }
                            TypeOrInferHandle::Infer(right_infer_type_handle) => {
                                parser.merge_infer_handles(
                                    left_infer_type_handle,
                                    right_infer_type_handle,
                                );
                                parser.expr_graph_add_edge(left_expr_handle, right_expr_handle);
                                parser.expr_graph_add_edge(right_expr_handle, left_expr_handle);
                            }
                        }
                    }
                }
                let expr_handle = parser.new_expr(self.handle);
                parser
                    .expr_type_map
                    .insert(expr_handle, type_reg.get_type_handle(&TypeInfo::Bool));

                parser.expr_to_analysis.insert(
                    expr_handle,
                    AnalysisExpr::Comparison {
                        lhs: left_expr_handle,
                        op,
                        rhs: right_expr_handle,
                        expr: expr_handle,
                    },
                );

                expr_handle
            }
            Expr::Assignment { left, op, right } => {
                let left_expr_handle = self.parse_ast_expr(*left, data);
                let right_expr_handle = self.parse_ast_expr(*right, data);

                match parser.get_type_or_infer(left_expr_handle) {
                    TypeOrInferHandle::Type(type_handle) => {
                        parser
                            .get_expected_expr_type(right_expr_handle, type_handle, data)
                            .unwrap_or_else(|found| {
                                blub_compile_error!(
                                    "expected right side to be {}, but was {}",
                                    type_reg.type_to_string(type_handle),
                                    type_reg.type_to_string(found)
                                )
                            });
                    }
                    TypeOrInferHandle::Infer(left_infer_type_handle) => {
                        match parser.get_type_or_infer(right_expr_handle) {
                            TypeOrInferHandle::Type(type_handle) => {
                                parser
                                    .get_expected_expr_type(left_expr_handle, type_handle, data)
                                    .unwrap_or_else(|found| {
                                        blub_compile_error!(
                                            "expected left side to be {}, but was {}",
                                            type_reg.type_to_string(type_handle),
                                            type_reg.type_to_string(found)
                                        )
                                    });
                            }
                            TypeOrInferHandle::Infer(right_infer_type_handle) => {
                                parser.merge_infer_handles(
                                    left_infer_type_handle,
                                    right_infer_type_handle,
                                );
                                parser.expr_graph_add_edge(left_expr_handle, right_expr_handle);
                                parser.expr_graph_add_edge(right_expr_handle, left_expr_handle);
                            }
                        }
                    }
                }
                let expr_handle = parser.new_expr(self.handle);
                parser
                    .expr_type_map
                    .insert(expr_handle, type_reg.get_type_handle(&TypeInfo::Unit));

                parser.expr_to_analysis.insert(
                    expr_handle,
                    AnalysisExpr::Assignment {
                        lhs: left_expr_handle,
                        op,
                        rhs: right_expr_handle,
                        expr: expr_handle,
                    },
                );

                expr_handle
            }

            Expr::Range { left, op, right } => {
                let left_expr_handle = self.parse_ast_expr(*left, data);
                let right_expr_handle = self.parse_ast_expr(*right, data);

                match parser.get_type_or_infer(left_expr_handle) {
                    TypeOrInferHandle::Type(type_handle) => {
                        parser
                            .get_expected_expr_type(right_expr_handle, type_handle, data)
                            .unwrap_or_else(|found| {
                                blub_compile_error!(
                                    "expected right side to be {}, but was {}",
                                    type_reg.type_to_string(type_handle),
                                    type_reg.type_to_string(found)
                                )
                            });
                    }
                    TypeOrInferHandle::Infer(left_infer_type_handle) => {
                        match parser.get_type_or_infer(right_expr_handle) {
                            TypeOrInferHandle::Type(type_handle) => {
                                parser
                                    .get_expected_expr_type(left_expr_handle, type_handle, data)
                                    .unwrap_or_else(|found| {
                                        blub_compile_error!(
                                            "expected left side to be {}, but was {}",
                                            type_reg.type_to_string(type_handle),
                                            type_reg.type_to_string(found)
                                        )
                                    });
                            }
                            TypeOrInferHandle::Infer(right_infer_type_handle) => {
                                parser.merge_infer_handles(
                                    left_infer_type_handle,
                                    right_infer_type_handle,
                                );
                                parser.expr_graph_add_edge(left_expr_handle, right_expr_handle);
                                parser.expr_graph_add_edge(right_expr_handle, left_expr_handle);
                            }
                        }
                    }
                }
                let expr_handle = parser.new_expr(self.handle);
                parser
                    .expr_type_map
                    .insert(expr_handle, type_reg.get_type_handle(&TypeInfo::Unit));

                parser.expr_to_analysis.insert(
                    expr_handle,
                    AnalysisExpr::Range {
                        lhs: left_expr_handle,
                        op,
                        rhs: right_expr_handle,
                        expr: expr_handle,
                    },
                );

                expr_handle
            }
            Expr::StructCreate {
                struct_ident,
                fields,
            } => {
                let struct_ident_expr_handle = self.parse_ast_expr(*struct_ident, data);
                let struct_ident_type_handle = parser.expr_to_type(struct_ident_expr_handle);
                let struct_ident_type_info = type_reg.get_type_info(struct_ident_type_handle);

                if struct_ident_type_info != TypeInfo::StaticType {
                    blub_compile_error!(
                        "struct creates requires a struct to create whcih has type of StaticType"
                    );
                }
                let struct_type_handle = parser.expr_to_static_type(struct_ident_expr_handle);
                let (struct_name, struct_type_fields) = type_reg
                    .get_type_info(struct_type_handle)
                    .into_struct()
                    .unwrap();
                match fields.len().cmp(&struct_type_fields.len()) {
                    std::cmp::Ordering::Less => blub_compile_error!("not enough fields are inited"),
                    std::cmp::Ordering::Greater => {
                        blub_compile_error!("too many fields are inited")
                    }
                    _ => (),
                }
                let mut inited_fields = HashSet::new();
                let mut analysis_fields = Vec::new();
                for (field_name, field_expr) in fields {
                    if inited_fields.contains(&field_name) {
                        blub_compile_error!("reinited field {}", field_name);
                    }
                    inited_fields.insert(field_name.clone());
                    let field_expr_handle = self.parse_ast_expr(field_expr, data);

                    let expected_type = struct_type_fields
                        .iter()
                        .find(|x| x.name == field_name)
                        .unwrap_or_else(|| {
                            blub_compile_error!(
                                "field {} is not on struct {}",
                                field_name,
                                struct_name
                            )
                        })
                        .type_handle;
                    parser
                        .get_expected_expr_type(field_expr_handle, expected_type, data)
                        .unwrap_or_else(|found| {
                            blub_compile_error!(
                                "field with name was inited with type {}, but expected {}",
                                type_reg.type_to_string(found),
                                type_reg.type_to_string(expected_type)
                            )
                        });

                    analysis_fields.push((field_name, field_expr_handle));
                }

                let expr_handle = parser.new_expr(self.handle);
                parser.expr_type_map.insert(expr_handle, struct_type_handle);
                parser.expr_to_analysis.insert(
                    expr_handle,
                    AnalysisExpr::StructCreate {
                        base: struct_ident_expr_handle,
                        args: analysis_fields,
                        expr: expr_handle,
                    },
                );
                expr_handle
            }
            Expr::Index { base, index } => {
                let expr_handle = parser.new_expr(self.handle);
                let base_expr_handle = self.parse_ast_expr(*base, data);
                let index_expr_handle = self.parse_ast_expr(*index, data);

                if parser
                    .copy_type_or_infer_handle(
                        base_expr_handle,
                        expr_handle,
                        Box::new(|handle, reg| {
                            reg.get_type_info(handle).into_array().unwrap_or_else(|_| {
                                blub_compile_error!("only arrays can be indexed")
                            })
                        }),
                        data,
                    )
                    .is_some()
                {
                    parser.add_infer_transformer(
                        expr_handle,
                        Box::new(|info, reg| {
                            reg.get_or_add_type(TypeInfo::Array {
                                handle: reg.get_type_handle(&info),
                            })
                        }),
                    );
                };

                parser
                    .get_expected_info_expr_type(
                        index_expr_handle,
                        TypeInfo::Number(NumberTypes::Usize),
                        data,
                    )
                    .unwrap_or_else(|found| {
                        blub_compile_error!(
                            "indexes into arrays need to have type usize, but was {}",
                            type_reg.type_to_string(found)
                        )
                    });

                parser.expr_to_analysis.insert(
                    expr_handle,
                    AnalysisExpr::Index {
                        base: base_expr_handle,
                        index: index_expr_handle,
                        expr: expr_handle,
                    },
                );

                expr_handle
            }
            Expr::Ref { is_mut, pointee } => {
                let pointee = self.parse_ast_expr(*pointee, data);

                let expr_handle = parser.new_expr(self.handle);
                if parser
                    .copy_type_or_infer_handle(
                        pointee,
                        expr_handle,
                        Box::new(move |handle, reg| {
                            reg.get_or_add_type(TypeInfo::Pointer {
                                is_mut,
                                pointee: handle,
                            })
                        }),
                        data,
                    )
                    .is_some()
                {
                    parser.add_infer_transformer(
                        expr_handle,
                        Box::new(|info, reg| match info {
                            TypeInfo::Pointer { is_mut: _, pointee } => pointee,
                            _ => blub_ice!(
                                "infer unpacking expected ref but got {}",
                                reg.info_to_string(info)
                            ),
                        }),
                    );
                };
                parser.expr_to_analysis.insert(
                    expr_handle,
                    AnalysisExpr::Ref {
                        is_mut,
                        pointee,
                        expr: expr_handle,
                    },
                );

                expr_handle
            }
            Expr::Group { inner } => {
                let inner_expr_handle = self.parse_ast_expr(*inner, data);
                let expr_handle = parser.clone_expr(inner_expr_handle, data);

                parser.expr_to_analysis.insert(
                    expr_handle,
                    AnalysisExpr::Group {
                        inner: inner_expr_handle,
                        expr: expr_handle,
                    },
                );

                expr_handle
            }
            Expr::ArrayInit { kind } => {
                let expr = parser.new_expr(self.handle);
                match kind {
                    ArrayInitExprKind::DefaultValue { init, count } => {
                        let init_expr_handle = self.parse_ast_expr(*init, data);
                        let count_expr_handle = self.parse_ast_expr(*count, data);

                        if parser
                            .copy_type_or_infer_handle(
                                init_expr_handle,
                                expr,
                                Box::new(|handle, reg| {
                                    reg.get_or_add_type(TypeInfo::Array { handle })
                                }),
                                data,
                            )
                            .is_some()
                        {
                            parser.add_infer_transformer(
                                expr,
                                Box::new(|info, reg| {
                                    info.into_array().unwrap_or_else(|info| {
                                        blub_compile_error!(
                                            "infer unpacking expected array but got {}",
                                            reg.info_to_string(info.clone())
                                        )
                                    })
                                }),
                            )
                        };
                        parser
                            .get_expected_info_expr_type(
                                count_expr_handle,
                                TypeInfo::Number(NumberTypes::Usize),
                                data,
                            )
                            .unwrap_or_else(|found| {
                                blub_compile_error!(
                                    "for initing struct with size, the size is expected to be usize but got {}",
                                    type_reg.type_to_string(found)
                                )
                            });
                        parser.expr_to_analysis.insert(
                            expr,
                            AnalysisExpr::ArrayInit {
                                kind: ArrayInitAnalysisKind::DefaultValue {
                                    value: init_expr_handle,
                                    count: count_expr_handle,
                                },
                                expr,
                            },
                        );
                    }
                    ArrayInitExprKind::InitItems { items } => {
                        let mut got_infer_or_type: Option<TypeOrInferHandle> = None;
                        let mut got_type_on_expr: Option<CodeExprHandle> = None;
                        let items = items.iter().cloned().map(|item| {
                            let item_expr_handle = self.parse_ast_expr(item, data);
                            match parser.get_type_or_infer(item_expr_handle) {
                                TypeOrInferHandle::Type(type_handle) => match got_infer_or_type {
                                    Some(x) => match x {
                                        TypeOrInferHandle::Type(got_type) => { // got item with known type and arr type is already known
                                            if got_type != type_handle {
                                                blub_compile_error!(
                                                    "in array init prev items were {} but got {}",
                                                    type_reg.type_to_string(got_type),
                                                    type_reg.type_to_string(type_handle)
                                                );
                                            }
                                        }
                                        TypeOrInferHandle::Infer(got_infer) => { // got item  with infer type and arr type is already known
                                            parser.set_type_on_infer(got_type_on_expr.unwrap(), got_infer, type_handle, data);
                                            got_infer_or_type =
                                                Some(TypeOrInferHandle::Type(type_handle));
                                        }
                                    },
                                    None => { // first item and it has a known type
                                        got_type_on_expr = Some(item_expr_handle);
                                        got_infer_or_type =
                                            Some(TypeOrInferHandle::Type(type_handle));
                                        let array_type = type_reg.get_or_add_type(TypeInfo::Array { handle: type_handle });
                                        parser.expr_type_map.insert(expr, array_type);
                                    }
                                },
                                TypeOrInferHandle::Infer(expr_infer) => match got_infer_or_type {
                                    Some(x) => match x {
                                        TypeOrInferHandle::Type(got_type) => { // got item with known type but arr type is infered
                                            got_type_on_expr = Some(item_expr_handle);
                                            parser.set_type_on_infer(item_expr_handle, expr_infer, got_type, data);
                                        }
                                        TypeOrInferHandle::Infer(got_infer) => { // got item with infered type and arr type is also infered
                                            parser.expr_graph_add_edge(expr, item_expr_handle);
                                            parser.merge_infer_handles(expr_infer, got_infer);
                                        }
                                    },
                                    None => { // got item with infered type and this is the first item
                                        parser.expr_graph_add_edge(expr, item_expr_handle);
                                        got_infer_or_type =
                                            Some(TypeOrInferHandle::Infer(expr_infer));
                                        parser.copy_type_or_infer_handle(
                                            item_expr_handle,
                                            expr,
                                            Box::new(|handle, reg| reg.get_or_add_type(TypeInfo::Array { handle })),
                                            data
                                        );
                                        parser.add_infer_transformer(
                                            expr,
                                            Box::new(|info, reg| {
                                                info.into_array().unwrap_or_else(|info| {
                                                    blub_compile_error!(
                                                        "infer unpacking expected array but got {}",
                                                        reg.info_to_string(info.clone())
                                                    )
                                                })
                                            }),
                                        );
                                    }
                                },
                            }
                            item_expr_handle
                        }).collect::<Vec<_>>();
                        parser.expr_to_analysis.insert(
                            expr,
                            AnalysisExpr::ArrayInit {
                                kind: ArrayInitAnalysisKind::InitItems { items },
                                expr,
                            },
                        );
                    }
                };
                expr
            }
            Expr::Unary { op, right } => {
                let right_expr_handle = self.parse_ast_expr(*right, data);
                let expr_handle = parser.new_expr(self.handle);

                parser.copy_type_or_infer_handle(
                    right_expr_handle,
                    expr_handle,
                    Box::new(|handle, _| handle),
                    data,
                );

                parser.expr_to_analysis.insert(
                    expr_handle,
                    AnalysisExpr::Unary {
                        op,
                        rhs: right_expr_handle,
                        expr: expr_handle,
                    },
                );

                expr_handle
            }
        }
    }
    pub fn parse_ast_stmt(&mut self, stmt: Stmt, data: &CodeAnalyzerData) {
        let type_reg = data.get_mut::<TypeRegistry>();
        let parser = data.get_mut::<CodeScopeParser>();
        match stmt.clone() {
            Stmt::VarDecl {
                name,
                is_mut: _,
                init_value,
            } => {
                if self.get_var_expr_handle(&name, parser).is_some() {
                    blub_compile_error!("redefinition of variable {}", name);
                }
                if init_value.is_none() {
                    blub_compile_error!("init values expected for variables");
                }
                let init_expr_handle = self.parse_ast_expr(init_value.unwrap(), data);
                self.var_name_to_expr.insert(name.clone(), init_expr_handle);
                self.stmts.push(AnalysisStmt::VarDecl {
                    stmt,
                    init_value: init_expr_handle,
                });
                parser.add_expr_debug_name(init_expr_handle, format!("variable {}", name));
            }
            Stmt::If { guard, body } => {
                let guard_expr_handle = self.parse_ast_expr(guard, data);
                let _ = parser.get_expected_info_expr_type(guard_expr_handle, TypeInfo::Bool, data);
                let new_scope_handle = parser.new_scope(Some(self.handle), self.in_fn.clone());
                let new_scope = parser.get_scope_mut(new_scope_handle);
                new_scope.parse_code_block(body, data);

                self.stmts.push(AnalysisStmt::If {
                    stmt,
                    scope: new_scope_handle,
                    guard: guard_expr_handle,
                });
            }
            Stmt::For {
                capture,
                iter,
                body,
            } => {
                let iter_expr_handle = self.parse_ast_expr(iter, data);

                let new_scope_handle = parser.new_scope(Some(self.handle), self.in_fn.clone());
                let mut capture_vars = Vec::new();

                let mut captured_var_exprs = Vec::new();

                match capture.clone() {
                    Expr::Ident(x) => {
                        capture_vars.push(x.clone());

                        let capture_expr_handle = parser.new_expr(new_scope_handle);
                        captured_var_exprs.push(AnalysisExpr::CaptureVar {
                            name: x.clone(),
                            is_mut: false,
                            var_expr: capture_expr_handle,
                            expr: capture_expr_handle,
                        });

                        parser
                            .get_scope_mut(new_scope_handle)
                            .var_name_to_expr
                            .insert(x.clone(), capture_expr_handle);
                        if let Some(_) = parser.copy_type_or_infer_handle(
                            iter_expr_handle,
                            capture_expr_handle,
                            Box::new(|handle, reg| match reg.get_type_info(handle) {
                                TypeInfo::Array { handle } => handle,
                                _ => {
                                    blub_compile_error!("only arrays support iteration for now")
                                }
                            }),
                            data,
                        ) {
                            parser.add_infer_transformer(
                                capture_expr_handle,
                                Box::new(|info, reg| {
                                    reg.get_or_add_type(TypeInfo::Array {
                                        handle: reg.get_type_handle(&info),
                                    })
                                }),
                            );
                        };
                        parser.expr_to_analysis.insert(
                            capture_expr_handle,
                            AnalysisExpr::CaptureVar {
                                name: x.clone(),
                                is_mut: false,
                                var_expr: capture_expr_handle,
                                expr: capture_expr_handle,
                            },
                        );
                    }
                    _ => blub_compile_error!("captures can only be idents for now"),
                };
                parser
                    .get_scope_mut(new_scope_handle)
                    .parse_code_block(body, data);
                self.stmts.push(AnalysisStmt::For {
                    stmt,
                    scope: new_scope_handle,
                    captures: captured_var_exprs,
                    iter_value: iter_expr_handle,
                });
            }
            Stmt::FuncDecl {
                name, args, body, ..
            } => {
                let new_scope_handle = parser.new_scope(Some(self.handle), Some(name.clone()));
                parser
                    .fn_name_to_code_scope
                    .insert(name.clone(), new_scope_handle);
                let fn_type_handle = type_reg.fn_name_to_handle.get(&name).cloned().unwrap();
                let fn_info = type_reg.get_type_info(fn_type_handle);
                let (_, type_fn_args) = fn_info.clone().into_fn().unwrap();

                for (i, arg) in args.iter().enumerate() {
                    let arg_type = type_fn_args[i];
                    let arg_expr_handle = parser.new_expr(self.handle);
                    parser.expr_type_map.insert(arg_expr_handle, arg_type);
                    parser.expr_to_analysis.insert(
                        arg_expr_handle,
                        AnalysisExpr::FnArg {
                            name: arg.name.clone(),
                            arg_type,
                            expr: arg_expr_handle,
                        },
                    );
                    parser
                        .get_scope_mut(new_scope_handle)
                        .var_name_to_expr
                        .insert(arg.name.clone(), arg_expr_handle);
                }
                parser
                    .get_scope_mut(new_scope_handle)
                    .parse_code_block(body, data);
                self.stmts.push(AnalysisStmt::FunctionDecl {
                    stmt,
                    fn_info,
                    scope: new_scope_handle,
                });
            }
            Stmt::StructDecl { name, fields, .. } => {
                let struct_type_handle = *type_reg.struct_name_to_handle.get(&name).unwrap();
                let struct_info = type_reg.get_type_info(struct_type_handle);
                let (_, type_struct_fields) = struct_info.into_struct().unwrap();

                let analysis_fields = fields
                    .iter()
                    .enumerate()
                    .map(|(i, ast_field)| AnalysisStructField {
                        name: ast_field.name.clone(),
                        type_handle: type_struct_fields[i].type_handle,
                        init_value: ast_field
                            .default_value
                            .clone()
                            .map(|x| self.parse_ast_expr(x, data)),
                    })
                    .collect::<Vec<_>>();
                self.stmts.push(AnalysisStmt::StructDecl {
                    stmt,
                    fields: analysis_fields,
                });
            }
            Stmt::ExprStmt(expr) => {
                let expr_handle = self.parse_ast_expr(expr, data);
                self.stmts.push(AnalysisStmt::ExprStmt {
                    stmt,
                    expr: expr_handle,
                });
            }
            Stmt::Retrun(expr) => {
                let expr_handle = self.parse_ast_expr(expr, data);
                let in_fn = self.in_fn.clone().unwrap();
                let (ret_type, _) = type_reg
                    .get_type_info(*type_reg.fn_name_to_handle.get(&in_fn).unwrap())
                    .into_fn()
                    .unwrap();

                if let Err(got) = parser.get_expected_expr_type(expr_handle, ret_type, data) {
                    blub_compile_error!(
                        "expected {} but got {}",
                        type_reg.type_to_string(ret_type),
                        type_reg.type_to_string(got)
                    );
                }

                self.stmts.push(AnalysisStmt::Return {
                    stmt,
                    expr: expr_handle,
                });
            }
        }
    }
}
#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct CodeScopeHandle(pub u64);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct InferTypeHandle(pub u64);

pub type InferApplyer = Box<dyn Fn(TypeHandle, &mut TypeRegistry) -> TypeHandle>;
pub type ExprGraph = DiGraphMap<CodeExprHandle, u8>;

#[allow(clippy::type_complexity)]
pub struct CodeScopeParser {
    pub fn_name_to_code_scope: HashMap<String, CodeScopeHandle>,
    pub next_expr_handle: u64,
    pub next_sub_expr_handle: u64,
    pub next_scope_handle: u64,
    pub next_infer_type_handle: u64,
    //
    pub infer_expr_map: HashMap<InferTypeHandle, HashSet<CodeExprHandle>>,
    pub expr_infer_validator_map: HashMap<CodeExprHandle, Box<dyn Fn(TypeInfo, &TypeRegistry)>>,
    pub expr_transformer_map:
        HashMap<CodeExprHandle, Box<dyn Fn(TypeInfo, &mut TypeRegistry) -> TypeHandle>>,

    pub expr_graph: ExprGraph,
    pub expr_infer_applyer_map: HashMap<CodeExprHandle, InferApplyer>,
    pub expr_infer_map: HashMap<CodeExprHandle, InferTypeHandle>,
    //
    pub expr_type_map: HashMap<CodeExprHandle, TypeHandle>,
    pub expr_to_analysis: HashMap<CodeExprHandle, AnalysisExpr>,
    pub expr_to_static_type: HashMap<CodeExprHandle, TypeHandle>,
    pub expr_to_code_scope: HashMap<CodeExprHandle, CodeScopeHandle>,
    //
    pub sub_expr_type_map: HashMap<CodeSubExprHandle, TypeHandle>,
    pub sub_expr_to_static_type: HashMap<CodeSubExprHandle, TypeHandle>,
    pub sub_expr_to_code_scope: HashMap<CodeSubExprHandle, CodeScopeHandle>,
    pub expr_debug_name: HashMap<CodeExprHandle, String>,
    //
    pub parent_map: HashMap<CodeScopeHandle, HashSet<CodeScopeHandle>>,
    //
    pub scopes: HashMap<CodeScopeHandle, CodeScope>,
}
#[allow(clippy::type_complexity)]
impl CodeScopeParser {
    pub fn get_stmt_by_handle(&self, handle: AnalysisStmtHandle) -> &AnalysisStmt {
        &self.get_scope_ref(handle.scope).stmts[handle.idx]
    }
    fn expr_map_recurse(
        &mut self,
        base_expr: CodeExprHandle,
        mut resolved_type: TypeHandle,
        data: &CodeAnalyzerData,
        infer_handle: InferTypeHandle,
        resolved_exprs: &mut HashSet<CodeExprHandle>,
    ) {
        resolved_exprs.insert(base_expr);
        let type_reg = data.get_mut::<TypeRegistry>();

        let edges = self.expr_graph.clone();
        let edges = edges.neighbors_directed(base_expr, Direction::Outgoing);

        if let Some(validator) = self.expr_infer_validator_map.get(&base_expr) {
            validator(type_reg.get_type_info(resolved_type), type_reg);
        }
        self.expr_type_map.insert(base_expr, resolved_type);
        if let Some(transformer) = self.expr_transformer_map.get(&base_expr) {
            resolved_type = transformer(type_reg.get_type_info(resolved_type), type_reg);
        }
        for edge in edges {
            if !resolved_exprs.contains(&edge) {
                self.expr_map_recurse(edge, resolved_type, data, infer_handle, resolved_exprs);
            }
        }
        self.expr_infer_map.remove(&base_expr);
        self.infer_expr_map
            .get_mut(&infer_handle)
            .unwrap()
            .remove(&base_expr);
    }
    pub fn set_type_on_infer(
        &mut self,
        expr: CodeExprHandle,
        infer_handle: InferTypeHandle,
        type_handle: TypeHandle,
        data: &CodeAnalyzerData,
    ) {
        let type_reg = data.get_mut::<TypeRegistry>();
        let mut resolved_exprs = HashSet::new();

        // Initial resolution wave based on the known type
        self.expr_map_recurse(expr, type_handle, data, infer_handle, &mut resolved_exprs);

        let mut iter_count = 0;
        loop {
            let mut resolved_in_pass = Vec::new();

            // Check if there are any expressions left to infer for this handle
            let expressions_to_infer = match self.infer_expr_map.get(&infer_handle) {
                Some(exprs) => exprs.clone(),
                None => break, // The inference set has been completely resolved
            };

            if expressions_to_infer.is_empty() {
                break;
            }

            // Find expressions that can be resolved in this pass
            for unresolved_expr in expressions_to_infer {
                // An expression can be resolved if one of its dependencies is now resolved.
                if let Some(resolved_dependency) = self
                    .expr_graph
                    .neighbors_directed(unresolved_expr, Direction::Outgoing)
                    .find(|dep| resolved_exprs.contains(dep))
                {
                    let applyer = self.expr_infer_applyer_map.get(&unresolved_expr).unwrap();
                    let resolved_type = applyer(self.expr_to_type(resolved_dependency), type_reg);
                    resolved_in_pass.push((unresolved_expr, resolved_type));
                }
            }

            // If no new expressions could be resolved in a full pass, we're done (or stuck).
            if resolved_in_pass.is_empty() {
                break;
            }

            // Apply the resolutions found in this pass
            for (expr_to_resolve, new_type) in resolved_in_pass {
                // Use expr_map_recurse to resolve this expression and its downstream dependencies.
                // Check if it's not already resolved to avoid redundant work.
                if !resolved_exprs.contains(&expr_to_resolve) {
                    self.expr_map_recurse(
                        expr_to_resolve,
                        new_type,
                        data,
                        infer_handle,
                        &mut resolved_exprs,
                    );
                }
            }

            iter_count += 1;
            if iter_count >= 300 {
                // This safety break prevents infinite loops in case of a logic error in the graph.
                blub_compile_error!("type inference iteration limit reached");
            }
        }
    }
    pub fn get_expected_expr_type(
        &mut self,
        expr: CodeExprHandle,
        expected_type: TypeHandle,
        data: &CodeAnalyzerData,
    ) -> Result<TypeHandle, TypeHandle> {
        if let Some(handle) = self.expr_type_map.get(&expr) {
            if *handle != expected_type {
                return Err(*handle);
            }
            return Ok(*handle);
        }
        if let Some(infer_handle) = self.expr_infer_map.get(&expr) {
            self.set_type_on_infer(expr, *infer_handle, expected_type, data);
        } else {
            blub_ice!("expr handle didnt have infer nor type handle");
        }

        Ok(expected_type)
    }
    pub fn get_expected_info_expr_type(
        &mut self,
        expr: CodeExprHandle,
        expected_info: TypeInfo,
        data: &CodeAnalyzerData,
    ) -> Result<TypeHandle, TypeHandle> {
        let type_reg = data.get::<TypeRegistry>();
        self.get_expected_expr_type(expr, type_reg.get_type_handle(&expected_info), data)
    }
    pub fn get_type_or_infer(&self, expr: CodeExprHandle) -> TypeOrInferHandle {
        if let Some(handle) = self.expr_type_map.get(&expr) {
            TypeOrInferHandle::Type(*handle)
        } else {
            TypeOrInferHandle::Infer(
                *self
                    .expr_infer_map
                    .get(&expr)
                    .unwrap_or_else(|| blub_ice!("expr didnt have type nor infer handle")),
            )
        }
    }
    pub fn copy_type_or_infer_handle(
        &mut self,
        src: CodeExprHandle,
        dest: CodeExprHandle,
        applyer: InferApplyer,
        data: &CodeAnalyzerData,
    ) -> Option<InferTypeHandle> {
        match self.get_type_or_infer(src) {
            TypeOrInferHandle::Type(type_handle) => {
                self.expr_type_map
                    .insert(dest, applyer(type_handle, data.get_mut::<TypeRegistry>()));
            }
            TypeOrInferHandle::Infer(infer_type_handle) => {
                self.expr_infer_map.insert(dest, infer_type_handle);
                self.infer_expr_map
                    .get_mut(&infer_type_handle)
                    .unwrap()
                    .insert(dest);
                self.expr_infer_applyer_map.insert(dest, applyer);
                self.expr_graph_add_edge(dest, src);
                return Some(infer_type_handle);
            }
        };
        None
    }
    pub fn add_expr_infer_validator(
        &mut self,
        expr: CodeExprHandle,
        validator: Box<dyn Fn(TypeInfo, &TypeRegistry)>,
    ) {
        self.expr_infer_validator_map.insert(expr, validator);
    }
    pub fn add_type_infer(&mut self, expr: CodeExprHandle, applyer: InferApplyer) {
        let infer_handle = InferTypeHandle(self.next_infer_type_handle);
        self.next_infer_type_handle += 1;

        self.expr_infer_map.insert(expr, infer_handle);
        self.expr_infer_applyer_map.insert(expr, applyer);
        self.infer_expr_map.insert(infer_handle, {
            let mut set = HashSet::new();
            set.insert(expr);
            set
        });
    }
    pub fn merge_infer_handles(&mut self, into: InferTypeHandle, from: InferTypeHandle) {
        if into == from {
            return;
        }
        for expr in self.infer_expr_map.get(&from).cloned().unwrap() {
            self.infer_expr_map.get_mut(&into).unwrap().insert(expr);
            self.expr_infer_map.insert(expr, into);
        }
        //self.infer_expr_map.remove(&from);
    }

    pub fn add_infer_transformer(
        &mut self,
        expr: CodeExprHandle,
        transformer: Box<dyn Fn(TypeInfo, &mut TypeRegistry) -> TypeHandle>,
    ) {
        self.expr_transformer_map.insert(expr, transformer);
    }
    pub fn parent_has_expr(&self, scope: CodeScopeHandle, expr: CodeExprHandle) -> bool {
        self.parent_map[&scope].contains(self.expr_to_code_scope.get(&expr).unwrap())
    }
    pub fn parent_has_sub_expr(&self, scope: CodeScopeHandle, sub_expr: CodeSubExprHandle) -> bool {
        self.parent_map[&scope].contains(self.sub_expr_to_code_scope.get(&sub_expr).unwrap())
    }
    pub fn expr_to_type(&self, expr: CodeExprHandle) -> TypeHandle {
        *self
            .expr_type_map
            .get(&expr)
            .unwrap_or_else(|| blub_compile_error!("type expected to be known by this point"))
    }
    pub fn sub_expr_to_type(&self, sub_expr: CodeSubExprHandle) -> TypeHandle {
        *self.sub_expr_type_map.get(&sub_expr).unwrap()
    }
    pub fn expr_to_static_type(&self, expr: CodeExprHandle) -> TypeHandle {
        *self.expr_to_static_type.get(&expr).unwrap_or_else(|| {
            blub_ice!(
                "expr {:?} did not have static type, {:?}",
                expr,
                self.expr_to_type(expr)
            )
        })
    }
    pub fn clone_expr(&mut self, handle: CodeExprHandle, dep: &CodeAnalyzerData) -> CodeExprHandle {
        let expr = self.new_expr(*self.expr_to_code_scope.get(&handle).unwrap());

        self.copy_type_or_infer_handle(handle, expr, Box::new(|type_handle, _| type_handle), dep);
        self.expr_to_analysis
            .insert(expr, self.get_analysis_expr(handle));
        if let Some(type_handle) = self.expr_to_static_type.get(&handle) {
            self.expr_to_static_type.insert(expr, *type_handle);
        }

        expr
    }
    pub fn sub_expr_to_static_type(&self, sub_expr: CodeSubExprHandle) -> TypeHandle {
        *self.sub_expr_to_static_type.get(&sub_expr).unwrap()
    }
    pub fn get_analysis_expr(&self, expr: CodeExprHandle) -> AnalysisExpr {
        self.expr_to_analysis.get(&expr).unwrap().clone()
    }
    pub fn scope_of_expr(&self, expr: CodeExprHandle) -> CodeScopeHandle {
        *self.expr_to_code_scope.get(&expr).unwrap()
    }
    pub fn scope_of_sub_expr(&self, sub_expr: CodeSubExprHandle) -> CodeScopeHandle {
        *self.sub_expr_to_code_scope.get(&sub_expr).unwrap()
    }
    pub fn sub_expr_from_expr(
        &mut self,
        scope: CodeScopeHandle,
        expr: CodeExprHandle,
    ) -> CodeSubExprHandle {
        let new_handle = self.new_sub_expr(scope);
        self.sub_expr_type_map
            .insert(new_handle, self.expr_to_type(expr));
        if let Some(static_type) = self.expr_to_static_type.get(&expr) {
            self.sub_expr_to_static_type
                .insert(new_handle, *static_type);
        }
        new_handle
    }
    pub fn new_expr(&mut self, scope: CodeScopeHandle) -> CodeExprHandle {
        let handle = CodeExprHandle(self.next_expr_handle);
        self.next_expr_handle += 1;
        self.expr_to_code_scope.insert(handle, scope);
        handle
    }
    pub fn new_sub_expr(&mut self, scope: CodeScopeHandle) -> CodeSubExprHandle {
        let handle = CodeSubExprHandle(self.next_sub_expr_handle);
        self.next_sub_expr_handle += 1;
        self.sub_expr_to_code_scope.insert(handle, scope);
        handle
    }
    pub fn new_scope(
        &mut self,
        parent: Option<CodeScopeHandle>,
        in_fn: Option<String>,
    ) -> CodeScopeHandle {
        let new_scope_handle = CodeScopeHandle(self.next_scope_handle);
        self.next_scope_handle += 1;
        self.parent_map.insert(new_scope_handle, HashSet::new());
        if let Some(parent) = parent {
            self.parent_map
                .get_mut(&new_scope_handle)
                .unwrap()
                .insert(parent);
            let parents_parents = self.parent_map.get(&parent).unwrap().clone();
            self.parent_map
                .get_mut(&new_scope_handle)
                .unwrap()
                .extend(parents_parents.iter());
        }
        self.scopes.insert(
            new_scope_handle,
            CodeScope::new(new_scope_handle, parent, in_fn),
        );
        new_scope_handle
    }
    pub fn new_root_scope(&mut self) -> &mut CodeScope {
        let handle = self.new_scope(None, None);
        self.get_scope_mut(handle)
    }
    pub fn get_scope_mut(&mut self, scope: CodeScopeHandle) -> &mut CodeScope {
        self.scopes.get_mut(&scope).unwrap()
    }
    pub fn get_scope_ref(&self, scope: CodeScopeHandle) -> &CodeScope {
        self.scopes.get(&scope).unwrap()
    }
    pub fn expr_graph_add_edge(&mut self, from: CodeExprHandle, to: CodeExprHandle) {
        self.expr_graph.add_edge(from, to, 0);
    }
    pub fn add_expr_debug_name(&mut self, expr: CodeExprHandle, name: String) {
        self.expr_debug_name.insert(expr, name);
    }
}
impl Default for CodeScopeParser {
    fn default() -> Self {
        Self {
            next_expr_handle: 1,
            next_sub_expr_handle: 1,
            next_scope_handle: 1,
            next_infer_type_handle: 1,
            expr_graph: Default::default(),
            expr_infer_map: Default::default(),
            infer_expr_map: Default::default(),
            expr_infer_validator_map: Default::default(),
            expr_transformer_map: Default::default(),
            expr_infer_applyer_map: Default::default(),
            expr_type_map: Default::default(),
            expr_to_analysis: Default::default(),
            expr_to_static_type: Default::default(),
            expr_to_code_scope: Default::default(),
            sub_expr_type_map: Default::default(),
            sub_expr_to_static_type: Default::default(),
            sub_expr_to_code_scope: Default::default(),
            expr_debug_name: Default::default(),
            parent_map: Default::default(),
            scopes: Default::default(),
            fn_name_to_code_scope: Default::default(),
        }
    }
}
/*





























*/
