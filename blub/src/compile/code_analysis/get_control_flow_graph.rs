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
            let end_node = AnalysisStmtHandle::INVALID;
            let last_conns = self.analyze_stmt(
                fn_body,
                AnalysisStmtHandle {
                    scope: *fn_scope,
                    idx: 0,
                },
                vec![],
                code_scope_parser,
                &mut graph,
            );
            graph.add_node(end_node);
            last_conns.iter().for_each(|prev| {
                graph.add_edge(*prev, end_node, ());
            });
        }
        self.graph_fn_map.insert(fn_name, graph);
    }
    pub fn check_all_paths_return(&mut self, fn_name: &String) -> bool {
        let graph = self.graph_fn_map.get(fn_name).unwrap();

        todo!()
    }
    /// _Returns:_ the statements which has a direct path to the next statement in the upper scope
    /// so for non conditional statements the last stmt in the scope
    /// and for if statements, the statement, and the last stmt conns in each code path
    fn analyze_stmt(
        &mut self,
        stmts: &[AnalysisStmt],
        current_handle: AnalysisStmtHandle,
        prev_conns: Vec<AnalysisStmtHandle>,
        code_scope_parser: &CodeScopeParser,
        graph: &mut ControlFlowGraph,
    ) -> Vec<AnalysisStmtHandle> {
        let current_stmt = &stmts[current_handle.idx];
        match current_stmt {
            AnalysisStmt::FunctionDecl { .. } => unreachable!(),
            AnalysisStmt::StructDecl { .. } => unreachable!(),
            AnalysisStmt::If { scope, .. } => {
                graph.add_node(current_handle);
                let body = &code_scope_parser.get_scope_ref(*scope).stmts;
                let last_conns_in_body = if !body.is_empty() {
                    Some(self.analyze_stmt(
                        body,
                        AnalysisStmtHandle {
                            scope: *scope,
                            idx: 0,
                        },
                        vec![current_handle],
                        code_scope_parser,
                        graph,
                    ))
                } else {
                    None
                };
                prev_conns.iter().for_each(|prev| {
                    graph.add_edge(*prev, current_handle, ());
                });
                let mut conns_to_next = vec![];
                if let Some(lasts) = last_conns_in_body {
                    conns_to_next.extend_from_slice(&lasts);
                }
                conns_to_next.push(current_handle);
                if let Some(_) = stmts.get(current_handle.idx + 1) {
                    return self.analyze_stmt(
                        stmts,
                        AnalysisStmtHandle {
                            scope: current_handle.scope,
                            idx: current_handle.idx + 1,
                        },
                        conns_to_next.clone(),
                        code_scope_parser,
                        graph,
                    );
                }
                return conns_to_next;
            }
            AnalysisStmt::For { scope, .. } => {
                graph.add_node(current_handle);
                prev_conns.iter().for_each(|prev| {
                    graph.add_edge(*prev, current_handle, ());
                });
                let body = &code_scope_parser.get_scope_ref(*scope).stmts;
                let last_conns_in_body = if !body.is_empty() {
                    Some(self.analyze_stmt(
                        body,
                        AnalysisStmtHandle {
                            scope: *scope,
                            idx: 0,
                        },
                        vec![current_handle],
                        code_scope_parser,
                        graph,
                    ))
                } else {
                    None
                };
                let mut conns_to_next = vec![];
                if let Some(lasts) = last_conns_in_body {
                    conns_to_next.extend_from_slice(&lasts);
                }
                conns_to_next.push(current_handle);
                if let Some(_) = stmts.get(current_handle.idx + 1) {
                    return self.analyze_stmt(
                        stmts,
                        AnalysisStmtHandle {
                            scope: current_handle.scope,
                            idx: current_handle.idx + 1,
                        },
                        conns_to_next.clone(),
                        code_scope_parser,
                        graph,
                    );
                }
                return conns_to_next;
            }
            AnalysisStmt::VarDecl { .. } | AnalysisStmt::ExprStmt { .. } => {
                graph.add_node(current_handle);
                prev_conns.iter().for_each(|prev| {
                    graph.add_edge(*prev, current_handle, ());
                });
                if let Some(_) = stmts.get(current_handle.idx + 1) {
                    return self.analyze_stmt(
                        stmts,
                        AnalysisStmtHandle {
                            scope: current_handle.scope,
                            idx: current_handle.idx + 1,
                        },
                        vec![current_handle],
                        code_scope_parser,
                        graph,
                    );
                } else {
                    return vec![current_handle];
                }
            }
            AnalysisStmt::Return { .. } => {
                graph.add_node(current_handle);
                prev_conns.iter().for_each(|prev| {
                    graph.add_edge(*prev, current_handle, ());
                });
                return vec![current_handle];
            }
        }
    }
}

/*




























*/
