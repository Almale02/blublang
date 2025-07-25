use std::{collections::HashMap, env::current_dir, fs::File, io::Write};

use petgraph::{
    algo,
    dot::{Config, Dot},
    prelude::DiGraphMap,
};

use crate::{
    blub_compile_error,
    compile::{
        code_analysis::code_scope::{
            ast_analysis::AnalysisStmt,
            code_scope::{AnalysisStmtHandle, CodeScopeParser},
        },
        types::type_registry::{TypeInfo, TypeRegistry},
    },
};

type ControlFlowGraph = DiGraphMap<AnalysisStmtHandle, ()>;
#[derive(Default)]
pub struct ControlFlowGraphs {
    pub graph_fn_map: HashMap<String, ControlFlowGraph>,
}
impl ControlFlowGraphs {
    pub fn check_functions_return_correctly(
        &mut self,
        code_scope_parser: &CodeScopeParser,
        type_reg: &TypeRegistry,
    ) {
        for (name, graph) in self.graph_fn_map.iter() {
            let (ret_type, _args) = type_reg
                .get_type_info(*type_reg.fn_name_to_handle.get(name).unwrap())
                .into_fn()
                .unwrap();
            if ret_type != type_reg.get_type_handle(&TypeInfo::Unit) {
                println!("checked {}", name);
                if name == "main" {
                    let dot = Dot::with_config(graph, &[Config::EdgeNoLabel]);
                    dbg!(graph.node_count());

                    let mut path = current_dir().unwrap();
                    path.push("main.dot");
                    dbg!(&path);

                    let mut file = File::create(&path).unwrap();
                    write!(file, "{:?}", dot).unwrap();
                }
                if !self.check_all_paths_return(code_scope_parser, name, graph) {
                    blub_compile_error!(
                        "function: {} have control flow paths which doesnt return correctly",
                        name
                    );
                }
            }
        }
    }
    pub fn check_all_paths_return(
        &self,
        code_scope_parser: &CodeScopeParser,
        fn_name: &String,
        graph: &ControlFlowGraph,
    ) -> bool {
        let fn_scope = code_scope_parser
            .fn_name_to_code_scope
            .get(fn_name)
            .unwrap();
        let fn_body = &code_scope_parser.get_scope_ref(*fn_scope).stmts;
        if fn_body.is_empty() {
            return false;
        }
        let start_node = AnalysisStmtHandle {
            scope: *fn_scope,
            idx: 0,
        };
        !algo::has_path_connecting(graph, start_node, AnalysisStmtHandle::INVALID, None)
    }
    pub fn create_control_flow_graphs(&mut self, code_scope_parser: &CodeScopeParser) {
        for (fn_name, fn_scope) in code_scope_parser.fn_name_to_code_scope.iter() {
            let mut graph = ControlFlowGraph::default();
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
            self.graph_fn_map.insert(fn_name.clone(), graph);
        }
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
            AnalysisStmt::If {
                base_case,
                elif_cases,
                else_case,
            } => {
                graph.add_node(current_handle);
                let mut conns_to_next = vec![];
                let base_body = &code_scope_parser.get_scope_ref(base_case.scope).stmts;
                if !base_body.is_empty() {
                    let last_conns_in_base_bod = self.analyze_stmt(
                        base_body,
                        AnalysisStmtHandle {
                            scope: base_case.scope,
                            idx: 0,
                        },
                        vec![current_handle],
                        code_scope_parser,
                        graph,
                    );
                    conns_to_next.extend_from_slice(&last_conns_in_base_bod);
                }
                prev_conns.iter().for_each(|prev| {
                    graph.add_edge(*prev, current_handle, ());
                });
                println!("before else is: {}", graph.node_count());
                if let Some(else_case) = else_case {
                    let else_body = &code_scope_parser.get_scope_ref(*else_case).stmts;
                    if !else_body.is_empty() {
                        let last_conns_in_else_body = self.analyze_stmt(
                            else_body,
                            AnalysisStmtHandle {
                                scope: *else_case,
                                idx: 0,
                            },
                            vec![current_handle],
                            code_scope_parser,
                            graph,
                        );

                        conns_to_next.extend_from_slice(&last_conns_in_else_body);
                    }
                } else {
                    conns_to_next.push(current_handle);
                };
                /*elif_cases.iter().for_each(|case| {
                    let elif_body = &code_scope_parser.get_scope_ref(case.scope).stmts;
                    if !elif_body.is_empty() {
                        let last_conns_in_elif_body = self.analyze_stmt(
                            elif_body,
                            AnalysisStmtHandle {
                                scope: case.scope,
                                idx: 0,
                            },
                            vec![current_handle],
                            code_scope_parser,
                            graph,
                        );

                        conns_to_next.extend_from_slice(&last_conns_in_elif_body);
                    }
                });*/
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
                return vec![];
            }
        }
    }
}

/*




























*/
