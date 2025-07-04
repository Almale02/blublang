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
        let mut graph = ControlFlowGraph::default();
        let code_scope_parser = analyzer_data.get::<CodeScopeParser>();
        let fn_scope = code_scope_parser
            .fn_name_to_code_scope
            .get(&fn_name)
            .unwrap();
        let fn_body = &code_scope_parser.get_scope_ref(*fn_scope).stmts;
        if !fn_body.is_empty() {
            self.analyze_stmt(
                fn_body,
                AnalysisStmtHandle {
                    scope: *fn_scope,
                    idx: 0,
                },
                None,
                code_scope_parser,
                &mut graph,
            );
        }
    }
    /// _Returns:_ last stmt in a scope
    fn analyze_stmt(
        &mut self,
        stmts: &[AnalysisStmt],
        current_handle: AnalysisStmtHandle,
        prev_handle: Option<AnalysisStmtHandle>,
        code_scope_parser: &CodeScopeParser,
        graph: &mut ControlFlowGraph,
    ) -> AnalysisStmtHandle {
        let current_stmt = &stmts[current_handle.idx];
        match current_stmt {
            AnalysisStmt::FunctionDecl { .. } => unreachable!(),
            AnalysisStmt::StructDecl { .. } => unreachable!(),
            AnalysisStmt::If { stmt, scope, guard } => {
                graph.add_node(current_handle);
                let body = &code_scope_parser.get_scope_ref(*scope).stmts;
                let mut last_in_body = if !body.is_empty() {
                    Some(self.analyze_stmt(
                        body,
                        AnalysisStmtHandle {
                            scope: *scope,
                            idx: 0,
                        },
                        None,
                        code_scope_parser,
                        graph,
                    ))
                } else {
                    None
                };
                if let Some(prev) = prev_handle {
                    graph.add_edge(prev, current_handle, ());
                }
            }
            AnalysisStmt::For {
                stmt,
                scope,
                captures,
                iter_value,
            } => {
                todo!()
            }
            AnalysisStmt::VarDecl { .. }
            | AnalysisStmt::ExprStmt { .. }
            | AnalysisStmt::Return { .. } => {
                graph.add_node(current_handle);
                if let Some(prev) = prev_handle {
                    graph.add_edge(prev, current_handle, ());
                }
                if let Some(_) = stmts.get(current_handle.idx + 1) {
                    return self.analyze_stmt(
                        stmts,
                        AnalysisStmtHandle {
                            scope: current_handle.scope,
                            idx: current_handle.idx + 1,
                        },
                        Some(current_handle),
                        code_scope_parser,
                        graph,
                    );
                } else {
                    return current_handle;
                }
            }
        }
    }
}

/*




























*/
