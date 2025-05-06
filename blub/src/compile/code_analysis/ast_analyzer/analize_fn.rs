use crate::{
    blub_compile_error,
    compile::{
        code_analysis::code_analyzer::AstAnalyzer,
        types::type_registry::{TypeInfo, TypeRegistry},
    },
};

use super::get_decls::GetDecl;

pub struct AnalyzeFunction;
impl AstAnalyzer for AnalyzeFunction {
    fn analize(
        &mut self,
        code_analyzer: &crate::compile::code_analysis::code_analyzer::CodeAnalyzer,
    ) {
        let type_reg = code_analyzer.data.get_mut::<TypeRegistry>();
        for fn_stmt in &code_analyzer.data.get::<GetDecl>().fns {
            let (_is_extern, _is_pub, name, args, _body, ret_type) =
                fn_stmt.clone().into_func_decl().unwrap();
            let type_info = TypeInfo::Fn {
                return_type: match ret_type {
                    Some(x) => type_reg.resolve_type_handle_from_ast_type(x).get_handle(),
                    None => type_reg.get_type_handle(&TypeInfo::Unit),
                },
                args: args
                    .iter()
                    .map(|arg| {
                        type_reg
                            .resolve_type_handle_from_ast_type(arg.arg_type.clone())
                            .get_handle()
                    })
                    .collect::<Vec<_>>(),
            };
            if type_reg.fn_name_to_handle.contains_key(&name) {
                blub_compile_error!("redefinition of function {}", name);
            }
            let type_handle = type_reg.get_or_add_type(type_info);
            type_reg.fn_name_to_handle.insert(name, type_handle);
        }
    }
}
