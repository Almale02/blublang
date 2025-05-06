use enum_as_inner::EnumAsInner;

use crate::compile::{
    lexer::token::{Number, Token},
    parser::ast::Stmt,
    types::type_registry::{TypeHandle, TypeInfo},
};

use super::code_scope::{CodeExprHandle, CodeScopeHandle};

#[derive(Debug, Clone, EnumAsInner)]
pub enum AnalysisExpr {
    Number(Number),
    String(String),
    Ident {
        name: String,
    },
    Access {
        base: CodeExprHandle,
        ident: String,
    },
    Call {
        base: CodeExprHandle,
        args: Vec<CodeExprHandle>,
    },
    StructCreate {
        base: CodeExprHandle,
        args: Vec<(String, CodeExprHandle)>,
    },
    Comparison {
        lhs: CodeExprHandle,
        op: Token,
        rhs: CodeExprHandle,
    },
    Assignment {
        lhs: CodeExprHandle,
        op: Token,
        rhs: CodeExprHandle,
    },
    Arithmetic {
        lhs: CodeExprHandle,
        op: Token,
        rhs: CodeExprHandle,
    },
    Range {
        lhs: CodeExprHandle,
        op: Token,
        rhs: CodeExprHandle,
    },
    FnArg {
        name: String,
        arg_type: TypeHandle,
    },
    CaptureVar {
        name: String,
        is_mut: bool,
        var_expr: CodeExprHandle,
    },
    Index {
        base: CodeExprHandle,
        index: CodeExprHandle,
    },
    Ref {
        is_mut: bool,
        pointee: CodeExprHandle,
    },
    Group {
        inner: CodeExprHandle,
    },
    ArrayInit {
        kind: ArrayInitAnalysisKind,
    },
}
#[derive(Debug, Clone, EnumAsInner)]
pub enum ArrayInitAnalysisKind {
    DefaultValue {
        value: CodeExprHandle,
        count: CodeExprHandle,
    },
    InitItems {
        items: Vec<CodeExprHandle>,
    },
}
#[derive(Debug, Clone, EnumAsInner)]
pub enum AnalysisStmt {
    VarDecl {
        stmt: Stmt,
        init_value: CodeExprHandle,
    },
    If {
        stmt: Stmt,
        scope: CodeScopeHandle,
        guard: CodeExprHandle,
    },
    For {
        stmt: Stmt,
        scope: CodeScopeHandle,
        captures: Vec<AnalysisExpr>,
        iter_value: CodeExprHandle,
    },
    FunctionDecl {
        stmt: Stmt,
        fn_info: TypeInfo,
        scope: CodeScopeHandle,
    },
    StructDecl {
        stmt: Stmt,
        fields: Vec<AnalysisStructField>,
    },
    ExprStmt {
        stmt: Stmt,
        expr: CodeExprHandle,
    },
}
#[derive(Debug, Clone)]
pub struct AnalysisStructField {
    pub name: String,
    pub type_handle: TypeHandle,
    pub init_value: Option<CodeExprHandle>,
}
