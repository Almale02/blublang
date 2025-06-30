use enum_as_inner::EnumAsInner;

use crate::compile::{
    lexer::token::{Number, Token},
    parser::ast::Stmt,
    types::type_registry::{TypeHandle, TypeInfo},
};

use super::code_scope::{CodeExprHandle, CodeScopeHandle};

#[derive(Debug, Clone, EnumAsInner)]
pub enum AnalysisExpr {
    Number {
        num: Number,
        expr: CodeExprHandle,
    },
    String {
        string: String,
        expr: CodeExprHandle,
    },
    Ident {
        name: String,
        expr: CodeExprHandle,
    },
    Access {
        base: CodeExprHandle,
        ident: String,
        expr: CodeExprHandle,
    },
    Call {
        base: CodeExprHandle,
        args: Vec<CodeExprHandle>,
        expr: CodeExprHandle,
    },
    StructCreate {
        base: CodeExprHandle,
        args: Vec<(String, CodeExprHandle)>,
        expr: CodeExprHandle,
    },
    Comparison {
        lhs: CodeExprHandle,
        op: Token,
        rhs: CodeExprHandle,
        expr: CodeExprHandle,
    },
    Assignment {
        lhs: CodeExprHandle,
        op: Token,
        rhs: CodeExprHandle,
        expr: CodeExprHandle,
    },
    Arithmetic {
        lhs: CodeExprHandle,
        op: Token,
        rhs: CodeExprHandle,
        expr: CodeExprHandle,
    },
    Range {
        lhs: CodeExprHandle,
        op: Token,
        rhs: CodeExprHandle,
        expr: CodeExprHandle,
    },
    FnArg {
        name: String,
        arg_type: TypeHandle,
        expr: CodeExprHandle,
    },
    CaptureVar {
        name: String,
        is_mut: bool,
        var_expr: CodeExprHandle,
        expr: CodeExprHandle,
    },
    Index {
        base: CodeExprHandle,
        index: CodeExprHandle,
        expr: CodeExprHandle,
    },
    Ref {
        is_mut: bool,
        pointee: CodeExprHandle,
        expr: CodeExprHandle,
    },
    Group {
        inner: CodeExprHandle,
        expr: CodeExprHandle,
    },
    ArrayInit {
        kind: ArrayInitAnalysisKind,
        expr: CodeExprHandle,
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
    Return {
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
