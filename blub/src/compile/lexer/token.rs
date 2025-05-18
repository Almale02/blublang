use std::fmt::{Display, Write};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenId {
    Eof,
    Number,
    String,
    Ident,

    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBlock,
    CloseBlock,
    Assignment,
    At,

    // Comparison operators
    Eq,
    Not,
    NotEq,
    Less,
    Greater,
    LessEq,
    GreaterEq,

    // Logical operators
    Or,
    And,

    // Punctuation
    Dot,
    Range,
    RangeIncl,
    SemiColon,
    Colon,
    Comma,
    Arrow,

    // Compound assignment operators
    PlusEq,
    MinusEq,

    // Arithmetic operators
    Plus,
    Minus,
    Div,
    Star,
    Percent,

    // Reserved keywords
    Let,
    Mut,
    Use,
    Fn,
    For,
    In,
    Loop,
    Match,
    If,
    Struct,
    Pub,
    Trait,
    Return,
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.to_id().fmt(f)
    }
}
impl Display for TokenId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            TokenId::Eof => "eof",
            TokenId::Number => "number",
            TokenId::String => "str",
            TokenId::Ident => "ident",
            TokenId::OpenParen => "(",
            TokenId::CloseParen => ")",
            TokenId::OpenBracket => "[",
            TokenId::CloseBracket => "]",
            TokenId::OpenBlock => "{",
            TokenId::CloseBlock => "}",
            TokenId::Assignment => "=",
            TokenId::At => "@",
            TokenId::Eq => "==",
            TokenId::Not => "!",
            TokenId::NotEq => "!=",
            TokenId::Less => "<",
            TokenId::Greater => ">",
            TokenId::LessEq => "<=",
            TokenId::GreaterEq => ">=",
            TokenId::Or => "or",
            TokenId::And => "and",
            TokenId::Dot => ".",
            TokenId::Range => "..",
            TokenId::RangeIncl => "..=",
            TokenId::SemiColon => ";",
            TokenId::Colon => ":",
            TokenId::Comma => ",",
            TokenId::Arrow => "->",
            TokenId::PlusEq => "+=",
            TokenId::MinusEq => "-=",
            TokenId::Plus => "+",
            TokenId::Minus => "-",
            TokenId::Div => "/",
            TokenId::Star => "*",
            TokenId::Percent => "%",
            TokenId::Let => "let",
            TokenId::Mut => "mut",
            TokenId::Use => "use",
            TokenId::Fn => "fn",
            TokenId::For => "for",
            TokenId::In => "in",
            TokenId::Loop => "loop",
            TokenId::Match => "match",
            TokenId::If => "if",
            TokenId::Struct => "struct",
            TokenId::Pub => "pub",
            TokenId::Trait => "trait",
            TokenId::Return => "return",
        })
    }
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Number {
    pub is_neg: bool,
    pub whole_seg: String,
    pub dec_seg: String,
    pub number_type: String,
}
impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_neg {
            let _ = f.write_char('-');
        }
        f.write_str(&self.whole_seg)
    }
}
#[derive(Clone, Debug, Eq, enum_as_inner::EnumAsInner)]
pub enum Token {
    Eof,
    Number(Number),
    String(String),
    Ident(String),

    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBlock,
    CloseBlock,
    Assignment,
    At,

    // Comparison operators
    Eq,
    Not,
    NotEq,
    Less,
    Greater,
    LessEq,
    GreaterEq,

    // Logical operators
    Or,
    And,

    // Punctuation
    Dot,
    Range,
    RangeIncl,
    SemiColon,
    Colon,
    Comma,
    Arrow,

    // Compound assignment operators
    PlusEq,
    MinusEq,

    // Arithmetic operators
    Plus,
    Minus,
    Div,
    Star,
    Percent,

    // Reserved keywords
    Let,
    Mut,
    Use,
    Fn,
    For,
    In,
    Loop,
    Match,
    If,
    Struct,
    Pub,
    Trait,
    Return,
}
impl Token {
    pub fn to_id(&self) -> TokenId {
        match self {
            Token::Eof => TokenId::Eof,
            Token::Number { .. } => TokenId::Number,
            Token::String(_) => TokenId::String,
            Token::Ident(_) => TokenId::Ident,
            Token::OpenParen => TokenId::OpenParen,
            Token::CloseParen => TokenId::CloseParen,
            Token::OpenBracket => TokenId::OpenBracket,
            Token::CloseBracket => TokenId::CloseBracket,
            Token::OpenBlock => TokenId::OpenBlock,
            Token::CloseBlock => TokenId::CloseBlock,
            Token::Assignment => TokenId::Assignment,
            Token::At => TokenId::At,
            Token::Eq => TokenId::Eq,
            Token::Not => TokenId::Not,
            Token::NotEq => TokenId::NotEq,
            Token::Less => TokenId::Less,
            Token::Greater => TokenId::Greater,
            Token::LessEq => TokenId::LessEq,
            Token::GreaterEq => TokenId::GreaterEq,
            Token::Or => TokenId::Or,
            Token::And => TokenId::And,
            Token::Dot => TokenId::Dot,
            Token::Range => TokenId::Range,
            Token::RangeIncl => TokenId::RangeIncl,
            Token::SemiColon => TokenId::SemiColon,
            Token::Colon => TokenId::Colon,
            Token::Comma => TokenId::Comma,
            Token::Arrow => TokenId::Arrow,
            Token::PlusEq => TokenId::PlusEq,
            Token::MinusEq => TokenId::MinusEq,
            Token::Plus => TokenId::Plus,
            Token::Minus => TokenId::Minus,
            Token::Div => TokenId::Div,
            Token::Star => TokenId::Star,
            Token::Percent => TokenId::Percent,
            Token::Let => TokenId::Let,
            Token::Mut => TokenId::Mut,
            Token::Use => TokenId::Use,
            Token::Fn => TokenId::Fn,
            Token::For => TokenId::For,
            Token::In => TokenId::In,
            Token::Loop => TokenId::Loop,
            Token::Match => TokenId::Match,
            Token::If => TokenId::If,
            Token::Struct => TokenId::Struct,
            Token::Pub => TokenId::Pub,
            Token::Trait => TokenId::Trait,
            Token::Return => TokenId::Return,
        }
    }
}
impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.to_id() == other.to_id()
    }
}
