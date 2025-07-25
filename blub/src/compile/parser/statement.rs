use crate::{
    blub_compile_error, blub_ice,
    compile::{
        lexer::token::{Token, TokenId},
        parser::ast::{ArgsDecl, IfStmtGuardCase},
    },
};

use super::{
    ast::{AstStructField, Stmt},
    parser::{BindingPower, ParseStmtRes, Parser},
};

impl Parser<'_> {
    pub fn parse_stmt(&mut self) -> Stmt {
        match self.handlers.stmt_lookup.get(&self.curr_token().to_id()) {
            Some(x) => x(self),
            None => {
                let expr = self.parse_expr(BindingPower::Default);
                if self.advance_expect_null(TokenId::SemiColon).is_none() {
                    blub_compile_error!(
                        "didnt find semi colon, but got {}\n{}",
                        self.curr_token(),
                        self.remainder()
                    );
                }
                Stmt::ExprStmt(expr)
            }
        }
    }
    pub fn parse_block(&mut self) -> Vec<Stmt> {
        let mut block = Vec::new();
        while self.curr_token().to_id() != TokenId::CloseBlock {
            block.push(self.parse_stmt());
        }
        block
    }
    pub fn parse_var_declare_stmt(&mut self) -> Stmt {
        self.advance();
        let is_mut = self.advance().to_id() == TokenId::Mut;
        if !is_mut {
            self.backtrack();
        }
        let name = match self.advance_expect(TokenId::Ident) {
            Token::Ident(x) => x,
            _ => unreachable!(),
        };
        self.advance();
        if self.curr_token().to_id() == TokenId::SemiColon {
            return Stmt::VarDecl {
                name,
                is_mut,
                init_value: None,
            };
        } else {
            self.backtrack();
        }
        self.advance_expect(TokenId::Assignment);
        let init_value = self.parse_expr(BindingPower::Default);
        self.advance_expect(TokenId::SemiColon);
        Stmt::VarDecl {
            name,
            is_mut,
            init_value: Some(init_value),
        }
    }
    pub fn parse_for_stmt(&mut self) -> Stmt {
        self.advance();
        let capture = self.parse_expr(BindingPower::Default);
        self.advance_expect(TokenId::In);
        let has_iter_grouping = self.advance_expect_null(TokenId::OpenParen).is_some();
        if !has_iter_grouping {
            self.backtrack();
            self.expr_breakers.insert(TokenId::OpenBlock);
        }
        let iter = self.parse_expr(BindingPower::Default);
        if has_iter_grouping {
            self.advance_expect(TokenId::CloseParen);
        } else {
            self.expr_breakers.remove(&TokenId::OpenBlock);
        }
        self.advance_expect(TokenId::OpenBlock);

        let body = self.parse_block();

        match self.advance_expect_null(TokenId::CloseBlock) {
            Some(_) => (),
            None => blub_ice!("after parsing block, current token wasnt CloseBlock"),
        };

        Stmt::For {
            capture,
            iter,
            body,
        }
    }
    /*
    if guard {

    } else if guard_2 {

    } else {

    }

    */
    pub fn parse_if_stmt(&mut self) -> Stmt {
        self.advance();
        let has_iter_grouping = self.advance_expect_null(TokenId::OpenParen).is_some();
        if !has_iter_grouping {
            self.backtrack();
            self.expr_breakers.insert(TokenId::OpenBlock);
        }
        let base_guard = self.parse_expr(BindingPower::Default);
        if has_iter_grouping {
            self.advance_expect(TokenId::CloseParen);
        } else {
            self.expr_breakers.remove(&TokenId::OpenBlock);
        }
        self.advance_expect(TokenId::OpenBlock);
        let base_body = self.parse_block();
        self.advance_expect(TokenId::CloseBlock);

        let mut elif_cases = Vec::new();
        let mut else_body = None;
        dbg!(self.curr_token().to_id());
        while self.curr_token().to_id() == TokenId::Else {
            self.advance();
            if self.curr_token().to_id() == TokenId::If {
                self.advance();
                let has_iter_grouping = self.advance_expect_null(TokenId::OpenParen).is_some();
                if !has_iter_grouping {
                    self.backtrack();
                    self.expr_breakers.insert(TokenId::OpenBlock);
                }
                let elif_guard = self.parse_expr(BindingPower::Default);
                if has_iter_grouping {
                    self.advance_expect(TokenId::CloseParen);
                } else {
                    self.expr_breakers.remove(&TokenId::OpenBlock);
                }
                self.advance_expect(TokenId::OpenBlock);
                let elif_body = self.parse_block();
                self.advance_expect(TokenId::CloseBlock);

                elif_cases.push(IfStmtGuardCase::new(elif_guard, elif_body));
            } else {
                self.advance_expect(TokenId::OpenBlock);
                else_body = Some(self.parse_block());
                self.advance_expect(TokenId::CloseBlock);
                break;
            }
        }

        Stmt::If {
            base_case: IfStmtGuardCase::new(base_guard, base_body),
            elif_cases,
            else_body,
        }
    }
    pub fn parse_struct_decl_stmt(&mut self) -> Stmt {
        let is_pub = if let Some(prev) = self.look_back() {
            prev.to_id() == TokenId::Pub
        } else {
            false
        };
        let mut fields = Vec::new();
        self.advance();
        let is_extern = self.curr_token().to_id() == TokenId::At;
        if is_extern {
            self.advance();
        }
        let struct_name = match self.advance_expect(TokenId::Ident) {
            Token::Ident(x) => x,
            _ => unreachable!(),
        };
        self.advance_expect(TokenId::OpenBlock);

        loop {
            if self.curr_token().to_id() == TokenId::CloseBlock {
                self.advance();
                break;
            }
            let is_field_pub = self.curr_token().to_id() == TokenId::Pub;
            if is_field_pub {
                self.advance();
            }
            let field_name = match self.advance_expect(TokenId::Ident) {
                Token::Ident(x) => x,
                _ => unreachable!(),
            };
            self.advance_expect(TokenId::Colon);
            let field_type = self.parse_type(BindingPower::Default);
            fields.push(AstStructField {
                is_pub: is_field_pub,
                name: field_name,
                field_type,
                default_value: None,
            });
            if self.curr_token().to_id() == TokenId::Comma {
                self.advance();
                // Check for trailing comma before CloseBlock
                if self.curr_token().to_id() == TokenId::CloseBlock {
                    self.advance();
                    break;
                }
            } else {
                self.advance_expect(TokenId::CloseBlock);
                break;
            }
        }
        Stmt::StructDecl {
            is_extern,
            is_pub,
            name: struct_name,
            fields,
        }
    }
    pub fn parse_fn_decl_stmt(&mut self) -> ParseStmtRes {
        let is_pub = if let Some(prev) = self.look_back() {
            prev.to_id() == TokenId::Pub
        } else {
            false
        };
        let mut args = Vec::new();
        self.advance();

        let is_extern = self.curr_token().to_id() == TokenId::At;
        if is_extern {
            self.advance();
        }

        let fn_name = match self.advance_expect(TokenId::Ident) {
            Token::Ident(x) => x,
            _ => unreachable!(),
        };
        self.advance_expect(TokenId::OpenParen);
        loop {
            if self.curr_token().to_id() == TokenId::CloseParen {
                self.advance();
                break;
            }
            let arg_name = match self.advance_expect(TokenId::Ident) {
                Token::Ident(x) => x,
                _ => unreachable!(),
            };
            self.advance_expect(TokenId::Colon);
            let arg_type = self.parse_type(BindingPower::Default);
            args.push(ArgsDecl {
                name: arg_name,
                arg_type,
            });
            if self.curr_token().to_id() == TokenId::Comma {
                self.advance();
            } else {
                self.advance_expect(TokenId::CloseParen);
                break;
            }
        }
        let mut return_type = None;
        let has_ret_type = self.advance_expect_null(TokenId::Colon).is_some();
        if has_ret_type {
            return_type = Some(self.parse_type(BindingPower::Default));
        } else {
            self.backtrack();
        }
        self.advance_expect(TokenId::OpenBlock);
        let body = self.parse_block();
        self.advance_expect(TokenId::CloseBlock);
        Stmt::FuncDecl {
            is_extern,
            is_pub,
            name: fn_name,
            args,
            body,
            return_type,
        }
    }
    pub fn parse_return_stmt(&mut self) -> ParseStmtRes {
        self.advance();

        if self.curr_token().to_id() == TokenId::SemiColon {
            self.advance();
            return Stmt::Retrun(None);
        }
        let expr = self.parse_expr(BindingPower::Default);
        self.advance_expect(TokenId::SemiColon);

        return Stmt::Retrun(Some(expr));
    }
}
/*








































*/
