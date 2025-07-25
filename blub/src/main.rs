#![allow(clippy::module_inception)]
#![feature(map_try_insert)]
#![deny(unused_imports)]

use std::{any::type_name, env::args, fs::read_to_string, path::PathBuf, process::exit};

use colored::Colorize;
use compile::{
    code_analysis::{
        ast_analyzer::{
            analize_fn::AnalyzeFunction, analyze_ast_struct::AnalyzeAstStruct,
            get_ast_block::GetAstBlock, get_decls::GetDecl, validate_stmt_level::ValidateStmtLevel,
        },
        code_analyzer::CodeAnalyzerData,
        code_scope::code_scope::CodeScopeParser,
    },
    lexer::lexer::{Lexer, LexerHandlers},
    parser::parser::{Parser, ParserHandlers},
    types::type_registry::TypeRegistry,
};

use crate::compile::code_analysis::get_control_flow_graph::ControlFlowGraphs;

pub mod compile;

fn build(code: String) {
    let mut lexer_handlers = LexerHandlers::new();
    lexer_handlers.set_handlers();
    let mut lexer = Lexer::new(&code, &lexer_handlers);
    lexer.tokenize();
    //
    let mut parser_handlers = ParserHandlers::default();
    parser_handlers.create_lookups();

    let mut parser = Parser::new(&parser_handlers, &lexer.tokens);
    parser.setup();
    let ast = parser.parse();
    dbg!(&ast);
    //
    //
    let code_analyzer_data = Box::leak(Box::new(CodeAnalyzerData::new(&ast)));
    code_analyzer_data.add_new(GetAstBlock::default());
    code_analyzer_data.add_new(GetDecl::default());
    code_analyzer_data.add_new(TypeRegistry::default());
    code_analyzer_data.add_new(CodeScopeParser::default());
    code_analyzer_data.add_new(ControlFlowGraphs::default());
    //
    let type_reg = code_analyzer_data.get_mut::<TypeRegistry>();
    type_reg.add_primitives();
    let code_scope_parser = code_analyzer_data.get_mut::<CodeScopeParser>();
    //
    let mut validate_stmt_level = ValidateStmtLevel;
    let mut analyze_fn = AnalyzeFunction;
    let mut analyze_struct = AnalyzeAstStruct;
    validate_stmt_level.analize(&code_analyzer_data);
    code_analyzer_data
        .get_mut::<GetAstBlock>()
        .analize(&code_analyzer_data);
    code_analyzer_data
        .get_mut::<GetDecl>()
        .analize(&code_analyzer_data);
    analyze_struct.analize(&code_analyzer_data);
    analyze_fn.analize(&code_analyzer_data);

    let root_scope = code_scope_parser.new_root_scope();

    root_scope.parse_code_block(ast.clone(), &code_analyzer_data);
    let control_flow_graph = code_analyzer_data.get_mut::<ControlFlowGraphs>();
    control_flow_graph.create_control_flow_graphs(code_scope_parser);
    control_flow_graph.check_functions_return_correctly(code_scope_parser, type_reg);
}

fn main() {
    match args().collect::<Vec<_>>()[1].as_str() {
        "build" => {
            if let Some(path_str) = args().collect::<Vec<_>>().get(2) {
                let path = PathBuf::from(path_str);
                if !path.exists() {
                    blub_tui_error!("file {:?} doesnt exists", path.to_str().unwrap());
                }
                let code = read_to_string(path).unwrap();
                build(code);
                //
                println!("{}", "finished".bright_green());
                exit(0);
            } else {
                blub_tui_error!("invalid args");
            }
        }
        "test" => {
            //
        }
        _ => {}
    }
}

#[macro_export]
macro_rules! blub_compile_error {
    ($($arg:tt)*) => {{
        use colored::Colorize;
        use std::backtrace::Backtrace;
        let msg = format!($($arg)*);
        println!("{}{}", "compile error: ".red(), msg.on_bright_red().bold());
        println!("{}", Backtrace::capture());
        std::process::exit(-1);

    }};
}
#[macro_export]
macro_rules! blub_compile_error_debug {
    ($($arg:tt)*) => {{
        use colored::Colorize;
        use std::backtrace::Backtrace;
        let msg = format!($($arg)*);
        dbg!("{}{}", "compile error: ".red(), msg.on_bright_red().bold());
        println!("{}", Backtrace::capture());
        std::process::exit(-1);

    }};
}
#[macro_export]
macro_rules! blub_tui_error {
    ($($arg:tt)*) => {{
        use std::backtrace::Backtrace;
        use colored::Colorize;
        let msg = format!($($arg)*);
        println!("{}{}", "tui error: ".red(), msg.on_bright_red().bold());
        println!("{}", Backtrace::capture());
        std::process::exit(-1);

    }};
}

#[macro_export]
macro_rules! blub_ice {
    ($($arg:tt)*) => {{
        use std::backtrace::Backtrace;
        use colored::Colorize;
        let msg = format!($($arg)*);
        println!(
            "{}{}",
            "internal compile error: ".purple(),
            msg.on_bright_red().bold()
        );
        println!("{}", Backtrace::capture());
        std::process::exit(-1);
    }};
}

#[derive(Default)]
pub struct Takeable<T> {
    value: Option<T>,
}

impl<T> Takeable<T> {
    pub fn new(value: T) -> Self {
        Takeable { value: Some(value) }
    }

    pub fn take(&mut self) -> Option<T> {
        self.value.take()
    }
    pub fn get(&self) -> &T {
        self.value.as_ref().unwrap_or_else(|| {
            panic!(
                "tried to access a taked value with type : {}",
                type_name::<T>()
            )
        })
    }

    pub fn get_mut(&mut self) -> &mut T {
        self.value.as_mut().unwrap_or_else(|| {
            panic!(
                "tried to access a taked value with type : {}",
                type_name::<T>()
            )
        })
    }
    pub fn has_value(&self) -> bool {
        self.value.is_some()
    }
}
impl<T: std::fmt::Debug> std::fmt::Debug for Takeable<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}
impl<T: Clone> Clone for Takeable<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
        }
    }
}
