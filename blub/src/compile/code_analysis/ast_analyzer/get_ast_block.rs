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
            Stmt::If {
                base_case,
                elif_cases,
                else_body,
            } => {
                blocks.blocks.push(base_case.body.clone());
                get_blocks(blocks, &base_case.body);
                elif_cases.iter().for_each(|x| {
                    blocks.blocks.push(x.body.clone());
                    get_blocks(blocks, &x.body);
                });
                if let Some(else_body) = else_body {
                    blocks.blocks.push(else_body.clone());
                    get_blocks(blocks, &else_body);
                }
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
