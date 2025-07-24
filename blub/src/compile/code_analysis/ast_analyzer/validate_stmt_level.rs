use crate::{
    blub_compile_error,
    compile::{code_analysis::code_analyzer::CodeAnalyzerData, parser::ast::Stmt},
};

pub struct ValidateStmtLevel;
impl ValidateStmtLevel {
    pub fn analize(&mut self, code_analyzer: &CodeAnalyzerData) {
        validate(code_analyzer.ast, true)
    }
}
fn validate(block: &[Stmt], is_top_level: bool) {
    for stmt in block {
        if is_top_level {
            if !stmt.to_id().is_top_level() {
                blub_compile_error!("{} is not top level statement", stmt.to_id());
            }
        } else if !stmt.to_id().is_block_level() {
            blub_compile_error!("{} is not block level statement", stmt.to_id());
        }
        match stmt {
            Stmt::VarDecl { .. } => (),
            Stmt::If {
                base_case,
                elif_cases,
                else_body,
            } => {
                validate(&base_case.body, false);
                elif_cases.iter().for_each(|case| {
                    validate(&case.body, false);
                });
                if let Some(else_body) = else_body {
                    validate(else_body, false);
                }
            }
            Stmt::For { body, .. } => validate(body, false),
            Stmt::FuncDecl { body, .. } => validate(body, false),
            Stmt::StructDecl { .. } => (),
            Stmt::ExprStmt(_) => (),
            Stmt::Retrun(_) => (),
        }
    }
}
