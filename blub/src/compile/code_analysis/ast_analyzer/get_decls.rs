use std::collections::HashMap;

use crate::{
    blub_compile_error,
    compile::{code_analysis::code_analyzer::AstAnalyzer, parser::ast::Stmt},
};

#[derive(Default)]
pub struct GetDecl {
    pub fns: Vec<Stmt>,
    pub structs: Vec<Stmt>,
    pub struct_map: HashMap<String, usize>,
}

impl AstAnalyzer for GetDecl {
    fn analize(
        &mut self,
        code_analyzer: &crate::compile::code_analysis::code_analyzer::CodeAnalyzer,
    ) {
        for stmt in code_analyzer.ast {
            match stmt.clone() {
                Stmt::FuncDecl { .. } => {
                    self.fns.push(stmt.clone());
                }
                Stmt::StructDecl { name, .. } => {
                    if self.struct_map.contains_key(&name) {
                        blub_compile_error!("struct with name {} is already defined", &name);
                    }
                    self.structs.push(stmt.clone());
                    self.struct_map.insert(name, self.structs.len() - 1);
                }
                _ => (),
            }
        }
    }
}
