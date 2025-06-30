use crate::compile::{code_analysis::code_analyzer::CodeAnalyzerData, parser::ast::Stmt};

#[derive(Default)]
pub struct GetAstBlock {
    pub blocks: Vec<Vec<Stmt>>,
}

impl GetAstBlock {
    pub fn analize(&mut self, code_analyzer: &CodeAnalyzerData) {
        self.blocks.push(code_analyzer.ast.to_vec());
        get_blocks(self, code_analyzer.ast);
    }
}
pub fn get_blocks(blocks: &mut GetAstBlock, stmts: &[Stmt]) {
    for stmt in stmts {
        match stmt {
            Stmt::If { body, .. } => {
                blocks.blocks.push(body.clone());
                get_blocks(blocks, body)
            }
            Stmt::For { body, .. } => {
                blocks.blocks.push(body.clone());
                get_blocks(blocks, body);
            }
            Stmt::FuncDecl { body, .. } => {
                blocks.blocks.push(body.clone());
                get_blocks(blocks, body);
            }
            _ => (),
        }
    }
}
