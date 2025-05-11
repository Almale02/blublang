use std::fmt::Display;

use enum_as_inner::EnumAsInner;

use crate::compile::lexer::token::{Number, Token};

#[derive(Clone, Debug, EnumAsInner)]
pub enum Expr {
    Number(Number),
    String(String),
    Ident(String),
    Call {
        base: Box<Expr>,
        args: Vec<Expr>,
    },
    Assignment {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Comparison {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Arithmetic {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Range {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Group {
        inner: Box<Expr>,
    },
    StructCreate {
        struct_ident: Box<Expr>,
        fields: Vec<(String, Expr)>,
    },
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
    },
    Ref {
        is_mut: bool,
        pointee: Box<Expr>,
    },
    Access {
        left: Box<Expr>,
        ident: String,
    },
    ArrayInit {
        kind: ArrayInitExprKind,
    },
}
#[derive(Clone, Debug, EnumAsInner)]
pub enum ArrayInitExprKind {
    DefaultValue { init: Box<Expr>, count: Box<Expr> },
    InitItems { items: Vec<Expr> },
}
impl Expr {
    pub fn to_id(&self) -> ExprId {
        match self {
            Expr::Number(_) => ExprId::Number,
            Expr::String(_) => ExprId::String,
            Expr::Ident(_) => ExprId::Ident,
            Expr::Call { .. } => ExprId::Call,
            Expr::StructCreate { .. } => ExprId::StructCreate,
            Expr::Index { .. } => ExprId::Index,
            Expr::Ref { .. } => ExprId::Ref,
            Expr::Access { .. } => ExprId::Access,
            Expr::Group { .. } => ExprId::Group,
            Expr::ArrayInit { .. } => ExprId::ArrayInit,
            Expr::Assignment { .. } => ExprId::Assignment,
            Expr::Comparison { .. } => ExprId::Comparison,
            Expr::Arithmetic { .. } => ExprId::Arithmetic,
            Expr::Range { .. } => ExprId::Range,
        }
    }
    pub fn is(&self, id: ExprId) -> bool {
        self.to_id() == id
    }
}
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExprId {
    Number,
    String,
    Ident,
    Call,
    Assignment,
    Comparison,
    Arithmetic,
    Range,
    StructCreate,
    Index,
    Ref,
    Access,
    Group,
    ArrayInit,
}
impl Display for ExprId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ExprId::Number => "number literal",
            ExprId::String => "string literal",
            ExprId::Ident => "ident",
            ExprId::Call => "call expr",
            ExprId::StructCreate => "struct create expr",
            ExprId::Index => "index expr",
            ExprId::Ref => "ref expr",
            ExprId::Access => "access expr",
            ExprId::Group => "group expr",
            ExprId::ArrayInit => "array init expr",
            ExprId::Assignment => "assignment expr",
            ExprId::Comparison => "comparison expr",
            ExprId::Arithmetic => "arithmetic expr",
            ExprId::Range => "range expr",
        })
    }
}
#[derive(Clone, Debug, EnumAsInner)]
pub enum Stmt {
    VarDecl {
        name: String,
        is_mut: bool,
        init_value: Option<Expr>,
    },
    If {
        guard: Expr,
        body: Vec<Stmt>,
    },
    For {
        capture: Expr,
        iter: Expr,
        body: Vec<Stmt>,
    },
    FuncDecl {
        is_extern: bool,
        is_pub: bool,
        name: String,
        args: Vec<ArgsDecl>,
        body: Vec<Stmt>,
        return_type: Option<AstType>,
    },
    StructDecl {
        is_extern: bool,
        is_pub: bool,
        name: String,
        fields: Vec<AstStructField>,
    },
    ExprStmt(Expr),
    Retrun(Expr),
}
impl Stmt {
    pub fn to_id(&self) -> StmtId {
        match self {
            Stmt::VarDecl { .. } => StmtId::VarDecl,
            Stmt::If { .. } => StmtId::If,
            Stmt::For { .. } => StmtId::For,
            Stmt::FuncDecl { .. } => StmtId::FuncDecl,
            Stmt::StructDecl { .. } => StmtId::StructDecl,
            Stmt::ExprStmt(_) => StmtId::ExprStmt,
            Stmt::Retrun(_) => StmtId::Return,
        }
    }
}
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum StmtId {
    VarDecl,
    If,
    For,
    FuncDecl,
    StructDecl,
    ExprStmt,
    Return,
}
impl StmtId {
    pub fn is_top_level(&self) -> bool {
        match self {
            StmtId::VarDecl => false,
            StmtId::If => false,
            StmtId::For => false,
            StmtId::FuncDecl => true,
            StmtId::StructDecl => true,
            StmtId::ExprStmt => false,
            StmtId::Return => false,
        }
    }
    pub fn is_block_level(&self) -> bool {
        match self {
            StmtId::VarDecl => true,
            StmtId::If => true,
            StmtId::For => true,
            StmtId::FuncDecl => false,
            StmtId::StructDecl => false,
            StmtId::ExprStmt => true,
            StmtId::Return => true,
        }
    }
}
impl Display for StmtId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            StmtId::VarDecl => "var decl stmt",
            StmtId::If => "if stmt",
            StmtId::For => "for loop",
            StmtId::FuncDecl => "func decl",
            StmtId::StructDecl => "struct decl",
            StmtId::ExprStmt => "expr stmt",
            StmtId::Return => "return stmt",
        })
    }
}
#[derive(Clone, Debug)]
pub struct ArgsDecl {
    pub name: String,
    pub arg_type: AstType,
}
#[derive(Clone, Debug)]
pub struct AstStructField {
    pub is_pub: bool,
    pub name: String,
    pub field_type: AstType,
    pub default_value: Option<Expr>,
}

#[derive(Clone, Debug, EnumAsInner)]
pub enum AstType {
    Symbol(String),
    Array(Box<AstType>),
    Pointer { is_mut: bool, pointee: Box<AstType> },
}
impl AstType {
    pub fn to_id(&self) -> AstTypeId {
        match self {
            AstType::Symbol(_) => AstTypeId::Symbol,
            AstType::Array(_) => AstTypeId::Array,
            AstType::Pointer { .. } => AstTypeId::Pointer,
        }
    }
    pub fn is(&self, id: AstTypeId) -> bool {
        self.to_id() == id
    }
}
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum AstTypeId {
    Symbol,
    Array,
    Pointer,
}
impl Display for AstTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            AstTypeId::Symbol => "symbol type",
            AstTypeId::Array => "array type",
            AstTypeId::Pointer => "pointer type",
        })
    }
}
