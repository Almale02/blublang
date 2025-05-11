use crate::{
    blub_compile_error,
    compile::{code_analysis::code_analyzer::AstAnalyzer, parser::ast::Stmt},
};

pub struct ValidateStmtLevel;
impl AstAnalyzer for ValidateStmtLevel {
    fn analize(
        &mut self,
        code_analyzer: &crate::compile::code_analysis::code_analyzer::CodeAnalyzer,
    ) {
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
            Stmt::If { body, .. } => validate(body, false),
            Stmt::For { body, .. } => validate(body, false),
            Stmt::FuncDecl { body, .. } => validate(body, false),
            Stmt::StructDecl { .. } => (),
            Stmt::ExprStmt(_) => (),
            Stmt::Retrun(_) => (),
        }
    }
}
