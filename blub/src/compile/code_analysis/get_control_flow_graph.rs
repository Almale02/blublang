use std::collections::HashMap;

use petgraph::prelude::DiGraphMap;

use crate::compile::code_analysis::{
    code_analyzer::CodeAnalyzerData,
    code_scope::{
        ast_analysis::AnalysisStmt,
        code_scope::{AnalysisStmtHandle, CodeScopeParser},
    },
};

type ControlFlowGraph = DiGraphMap<AnalysisStmtHandle, ()>;
pub struct ControlFlowGraphs {
    pub graph_fn_map: HashMap<String, ControlFlowGraph>,
}
impl ControlFlowGraphs {
    pub fn create_control_flow_graph_for_fn(
        &mut self,
        analyzer_data: &mut CodeAnalyzerData,
        fn_name: String,
    ) {
        let graph = ControlFlowGraph::default();
        let code_scope_parser = analyzer_data.get::<CodeScopeParser>();
        let fn_body = &code_scope_parser
            .get_scope_ref(
                *code_scope_parser
                    .fn_name_to_code_scope
                    .get(&fn_name)
                    .unwrap(),
            )
            .stmts;
    }
    fn analyze_stmt(
        &mut self,
        stmts: &[AnalysisStmt],
        current_handle: AnalysisStmtHandle,
        prev_handle: Option<AnalysisStmtHandle>,
        code_scope_parser: &CodeScopeParser,
        graph: &mut ControlFlowGraph,
    ) {
        let current_stmt = &stmts[current_handle.idx];
        match current_stmt {
            AnalysisStmt::FunctionDecl {
                stmt,
                fn_info,
                scope,
            } => unreachable!(),
            AnalysisStmt::StructDecl { stmt, fields } => unreachable!(),
            AnalysisStmt::If { stmt, scope, guard } => todo!(),
            AnalysisStmt::For {
                stmt,
                scope,
                captures,
                iter_value,
            } => todo!(),
            AnalysisStmt::Return { stmt, expr } => todo!(),
            AnalysisStmt::VarDecl { stmt, .. } | AnalysisStmt::ExprStmt { stmt, .. } => {
                graph.add_node(current_handle);
                if let Some(prev) = prev_handle {
                    graph.add_edge(prev, current_handle, ());
                }
            }
        }
    }
}

/*




























*/
