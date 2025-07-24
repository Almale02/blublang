#![allow(clippy::redundant_closure)]

use std::collections::{HashMap, HashSet};

use crate::{
    blub_compile_error,
    compile::lexer::token::{Token, TokenId},
};

use super::ast::{AstType, Expr, Stmt};

pub type ParseExprRes = Expr;
pub type ParseStmtRes = Stmt;
pub type ParseAstTypeRes = AstType;
pub type NudHandler = Box<dyn Fn(&mut Parser) -> ParseExprRes>;
pub type LedHandler = Box<dyn Fn(&mut Parser, Expr, BindingPower) -> ParseExprRes>;
pub type StmtHandler = Box<dyn Fn(&mut Parser) -> ParseStmtRes>;
pub type TypeNudHandler = Box<dyn Fn(&mut Parser) -> ParseAstTypeRes>;
pub type TypeLedHandler = Box<dyn Fn(&mut Parser, AstType, BindingPower) -> ParseAstTypeRes>;

#[derive(Default)]
pub struct ParserHandlers {
    pub binding_powers: HashMap<TokenId, BindingPower>,
    pub nud_lookup: HashMap<TokenId, NudHandler>,
    pub led_lookup: HashMap<TokenId, LedHandler>,
    //
    pub type_binding_powers: HashMap<TokenId, BindingPower>,
    pub type_nud_lookup: HashMap<TokenId, TypeNudHandler>,
    pub type_led_lookup: HashMap<TokenId, TypeLedHandler>,
    //
    pub stmt_lookup: HashMap<TokenId, StmtHandler>,
}
impl ParserHandlers {
    pub fn nud(&mut self, token: TokenId, handler: NudHandler) {
        self.nud_lookup.insert(token, handler);
    }
    pub fn led(&mut self, token: TokenId, bp: BindingPower, handler: LedHandler) {
        self.led_lookup.insert(token, handler);
        self.binding_powers.insert(token, bp);
    }
    pub fn stmt(&mut self, token: TokenId, handler: StmtHandler) {
        self.stmt_lookup.insert(token, handler);
    }
    pub fn type_nud(&mut self, token: TokenId, handler: TypeNudHandler) {
        self.type_nud_lookup.insert(token, handler);
    }
    pub fn type_led(&mut self, token: TokenId, bp: BindingPower, handler: TypeLedHandler) {
        self.type_led_lookup.insert(token, handler);
        self.type_binding_powers.insert(token, bp);
    }
    #[rustfmt::skip]
    pub fn create_lookups(&mut self) {

        self.led(TokenId::And, BindingPower::Logical, Box::new(|parser, expr, bp| Parser::parse_arithmetic_expr(parser, expr, bp)));
        self.led(TokenId::Or, BindingPower::Logical, Box::new(|parser, expr, bp| Parser::parse_arithmetic_expr(parser, expr, bp)) );
        //
        self.led(TokenId::Less, BindingPower::Relational, Box::new(|parser, expr, bp| Parser::parse_comparison_expr(parser, expr, bp)) );
        self.led(TokenId::LessEq, BindingPower::Relational, Box::new(|parser, expr, bp| Parser::parse_comparison_expr(parser, expr, bp)) );
        self.led(TokenId::Greater, BindingPower::Relational, Box::new(|parser, expr, bp| Parser::parse_comparison_expr(parser, expr, bp)) );
        self.led(TokenId::GreaterEq, BindingPower::Relational, Box::new(|parser, expr, bp| Parser::parse_comparison_expr(parser, expr, bp)) );
        self.led(TokenId::Eq, BindingPower::Relational, Box::new(|parser, expr, bp| Parser::parse_comparison_expr(parser, expr, bp)) );
        self.led(TokenId::NotEq, BindingPower::Relational, Box::new(|parser, expr, bp| Parser::parse_comparison_expr(parser, expr, bp)) );
        //
        self.led(TokenId::Plus, BindingPower::Additive, Box::new(|parser, expr, bp| Parser::parse_arithmetic_expr(parser, expr, bp)) );
        self.led(TokenId::Minus, BindingPower::Additive, Box::new(|parser, expr, bp| Parser::parse_arithmetic_expr(parser, expr, bp)) );

        self.led(TokenId::Star, BindingPower::Multiplicative, Box::new(|parser, expr, bp| Parser::parse_arithmetic_expr(parser, expr, bp)) );
        self.led(TokenId::Div, BindingPower::Multiplicative, Box::new(|parser, expr, bp| Parser::parse_arithmetic_expr(parser, expr, bp)) );
        self.led(TokenId::Percent, BindingPower::Multiplicative, Box::new(|parser, expr, bp| Parser::parse_arithmetic_expr(parser, expr, bp)) );
        //
        self.led(TokenId::Assignment, BindingPower::Assignment, Box::new(|parser, expr, bp| Parser::parse_assignment_expr(parser, expr, bp)) );
        self.led(TokenId::PlusEq, BindingPower::Assignment, Box::new(|parser, expr, bp| Parser::parse_assignment_expr(parser, expr, bp)) );
        self.led(TokenId::MinusEq, BindingPower::Assignment, Box::new(|parser, expr, bp| Parser::parse_assignment_expr(parser, expr, bp)) );
        //
        self.led(TokenId::RangeIncl, BindingPower::Range, Box::new(|parser, expr, bp| Parser::parse_range_expr(parser, expr, bp)) );
        self.led(TokenId::Range, BindingPower::Range, Box::new(|parser, expr, bp| Parser::parse_range_expr(parser, expr, bp)) );
        //
        self.led(TokenId::Dot, BindingPower::Member, Box::new(|parser, expr, bp| Parser::parse_access(parser, expr, bp)) );
        //
        self.led(TokenId::OpenParen, BindingPower::Call, Box::new(|parser, expr, bp| Parser::parse_call(parser, expr, bp)) );
        self.led(TokenId::OpenBlock, BindingPower::Primary, Box::new(|parser, expr, bp| Parser::parse_struct_create(parser, expr, bp)) );
        self.led(TokenId::OpenBracket, BindingPower::Call, Box::new(|parser, expr, bp| Parser::parse_index(parser, expr, bp)) );
        //
        //
        self.stmt(TokenId::Return, Box::new(|x| Parser::parse_return_stmt(x)));
        self.stmt(TokenId::Let, Box::new(|x| Parser::parse_var_declare_stmt(x)));
        self.stmt(TokenId::For, Box::new(|x| Parser::parse_for_stmt(x)));
        self.stmt(TokenId::If, Box::new(|x| Parser::parse_if_stmt(x)));
        self.stmt(TokenId::Struct, Box::new(|x| Parser::parse_struct_decl_stmt(x)));
        self.stmt(TokenId::Fn, Box::new(|x| Parser::parse_fn_decl_stmt(x)));
        //
        //
        self.nud(TokenId::Not, Box::new(|x| Parser::parse_prefix_expr(x)));
        self.nud(TokenId::And, Box::new(|x| Parser::parse_ref_expr(x)));
        self.nud(TokenId::Number, Box::new(|x| Parser::parse_literal_expr(x)));
        self.nud(TokenId::String, Box::new(|x| Parser::parse_literal_expr(x)));
        self.nud(TokenId::Ident, Box::new(|x| Parser::parse_ident(x)));
        self.nud(TokenId::OpenParen, Box::new(|x| Parser::parse_group_expr(x)));
        self.nud(TokenId::OpenBracket, Box::new(|x| Parser::parse_array_init(x)));
        //
        //
        //
        self.type_nud(TokenId::Ident, Box::new(|x| Parser::parse_symbol_type(x)));
        self.type_nud(TokenId::Star, Box::new(|x| Parser::parse_ptr_type(x)));
        self.type_nud(TokenId::OpenBracket, Box::new(|x| Parser::parse_array_type(x)));
    }
}

pub struct Parser<'a> {
    pub tokens: &'a [Token],
    pub head: usize,
    //
    pub mod_tokens: HashSet<TokenId>,
    //
    pub handlers: &'a ParserHandlers,
    //
    pub expr_breakers: HashSet<TokenId>,
}
impl<'a> Parser<'a> {
    pub fn new(handlers: &'a ParserHandlers, tokens: &'a [Token]) -> Self {
        Self {
            handlers,
            tokens,
            mod_tokens: HashSet::new(),
            expr_breakers: HashSet::new(),
            head: 0,
        }
    }
    pub fn parse(&mut self) -> Vec<Stmt> {
        self.head = 0;
        if self.mod_tokens.contains(&self.curr_token().to_id()) {
            self.advance();
        }
        let mut body = Vec::new();
        while !self.has_finished() {
            body.push(self.parse_stmt());
        }
        body
    }
    pub fn curr_token(&self) -> Token {
        self.tokens[self.head].clone()
    }
    pub fn has_finished(&self) -> bool {
        self.head == self.tokens.len() - 1
    }
    pub fn remainder(&self) -> String {
        let mut string = String::new();

        for token in self.tokens {
            string = format!("{}\n{}", string, token.to_id());
        }
        string
    }
    pub fn advance(&mut self) -> Token {
        if self.has_finished() {
            blub_compile_error!("reached eof before expected");
        }
        let curr_token = self.curr_token();
        loop {
            self.head += 1;
            if !self.mod_tokens.contains(&self.curr_token().to_id()) {
                break;
            }
        }
        curr_token
    }
    pub fn advance_expect(&mut self, expected: TokenId) -> Token {
        let token = self.advance();
        if token.to_id() != expected {
            blub_compile_error!("expected token {}, but got {}", expected, token)
        }
        token
    }
    pub fn advance_expect_null(&mut self, expected: TokenId) -> Option<Token> {
        let token = self.advance();
        if token.to_id() != expected {
            return None;
        }
        Some(token)
    }
    pub fn backtrack(&mut self) -> Option<Token> {
        if self.head == 0 {
            return None;
        }
        self.head -= 1;
        Some(self.curr_token())
    }
    pub fn look_back(&self) -> Option<Token> {
        if self.head == 0 {
            return None;
        }
        Some(self.tokens[self.head - 1].clone())
    }
    pub fn look_ahead(&self) -> Option<Token> {
        if self.tokens.len() - 1 == self.head {
            return None;
        }
        Some(self.tokens[self.head + 1].clone())
    }
    pub fn is_prev(&self, id: TokenId) -> bool {
        if let Some(token) = self.look_back() {
            return token.to_id() == id;
        }
        false
    }
    pub fn is_next(&self, id: TokenId) -> bool {
        if let Some(token) = self.look_ahead() {
            return token.to_id() == id;
        }
        false
    }
    pub fn setup(&mut self) {
        self.mod_tokens.insert(TokenId::Pub);
    }
}
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum BindingPower {
    Default = 0,
    Comma,
    Assignment,
    Range,
    Logical,
    Relational,
    Additive,
    Multiplicative,
    Unary,
    Call,
    Member,
    Primary,
}
