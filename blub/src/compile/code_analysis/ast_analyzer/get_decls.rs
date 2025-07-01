use std::collections::HashMap;

use crate::{
    blub_compile_error,
    compile::{code_analysis::code_analyzer::CodeAnalyzerData, parser::ast::Stmt},
};

#[derive(Default)]
pub struct GetDecl {
    pub fns: Vec<Stmt>,
    pub fn_map: HashMap<String, usize>,
    pub structs: Vec<Stmt>,
    pub struct_map: HashMap<String, usize>,
}

impl GetDecl {
    pub fn analize(&mut self, code_analyzer: &CodeAnalyzerData) {
        for stmt in code_analyzer.ast {
            match stmt.clone() {
                Stmt::FuncDecl { name, .. } => {
                    self.fns.push(stmt.clone());
                    self.fn_map.insert(name, self.fns.len() - 1);
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
