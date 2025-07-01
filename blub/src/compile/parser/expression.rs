use crate::{
    blub_compile_error, blub_ice,
    compile::lexer::token::{Token, TokenId},
};

use super::{
    ast::{ArrayInitExprKind, Expr, ExprId},
    parser::{BindingPower, ParseExprRes, Parser},
};

impl Parser<'_> {
    pub fn parse_expr(&mut self, bp: BindingPower) -> ParseExprRes {
        if self.mod_tokens.contains(&self.curr_token().to_id()) {
            self.advance();
        }
        let token = self.curr_token();
        if token.to_id() == TokenId::SemiColon {
            blub_compile_error!("semicolons should not be parsed as experssions");
        }
        let nud_handler = match self.handlers.nud_lookup.get(&token.to_id()) {
            Some(x) => x,
            None => blub_compile_error!("expected nud handler for token: {}", token),
        };
        let mut left = nud_handler(self);
        if self.curr_token().to_id() == TokenId::SemiColon {
            return left;
        }
        while !self.expr_breakers.contains(&self.curr_token().to_id()) {
            let token_bp = match self.handlers.binding_powers.get(&self.curr_token().to_id()) {
                Some(x) => x.clone(),
                None => break,
            };
            if token_bp < bp {
                break;
            }
            if let Some(led_handler) = self.handlers.led_lookup.get(&self.curr_token().to_id()) {
                left = led_handler(self, left, bp.clone())
            }
        }
        left
    }
    pub fn parse_comparison_expr(&mut self, left: Expr, bp: BindingPower) -> ParseExprRes {
        let op = self.advance();
        let right = self.parse_expr(bp);

        Expr::Comparison {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }
    pub fn parse_assignment_expr(&mut self, left: Expr, bp: BindingPower) -> ParseExprRes {
        let op = self.advance();
        let right = self.parse_expr(bp);

        Expr::Assignment {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }
    pub fn parse_arithmetic_expr(&mut self, left: Expr, bp: BindingPower) -> ParseExprRes {
        let op = self.advance();
        let right = self.parse_expr(bp);

        Expr::Arithmetic {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }
    pub fn parse_range_expr(&mut self, left: Expr, bp: BindingPower) -> ParseExprRes {
        let op = self.advance();
        let right = self.parse_expr(bp);

        Expr::Range {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }
    pub fn parse_literal_expr(&mut self) -> ParseExprRes {
        let token = self.advance();
        match token {
            Token::Number(x) => Expr::Number(x),
            Token::String(x) => Expr::String(x),
            _ => blub_ice!("token {} is not a literal", token),
        }
    }
    pub fn parse_prefix_expr(&mut self) -> ParseExprRes {
        let op = self.advance();
        let right = self.parse_expr(BindingPower::Unary);
        Expr::Unary {
            op,
            right: Box::new(right),
        }
    }
    pub fn parse_ident(&mut self) -> ParseExprRes {
        Expr::Ident(self.advance_expect(TokenId::Ident).into_ident().unwrap())
    }
    pub fn parse_access(&mut self, left: Expr, _bp: BindingPower) -> ParseExprRes {
        self.advance_expect(TokenId::Dot);
        let ident = self.parse_ident().into_ident().unwrap();
        ParseExprRes::Access {
            left: Box::new(left),
            ident,
        }
    }
    pub fn parse_ref_expr(&mut self) -> ParseExprRes {
        self.advance();
        let is_mut = if self.curr_token().to_id() == TokenId::Mut {
            self.advance();
            true
        } else {
            false
        };
        let pointee = self.parse_expr(BindingPower::Default);

        Expr::Ref {
            is_mut,
            pointee: Box::new(pointee),
        }
    }
    pub fn parse_call(&mut self, left: Expr, bp: BindingPower) -> ParseExprRes {
        self.advance_expect(TokenId::OpenParen);
        let mut args = Vec::new();
        loop {
            if self.curr_token().to_id() != TokenId::CloseParen {
                let new_expr = self.parse_expr(bp.clone());
                args.push(new_expr);
            }
            if self.advance().to_id() != TokenId::CloseParen {
                self.backtrack();
                self.advance_expect(TokenId::Comma);
            } else {
                return Expr::Call {
                    base: Box::new(left),
                    args,
                };
            }
        }
    }
    pub fn parse_index(&mut self, left: Expr, _bp: BindingPower) -> ParseExprRes {
        self.advance_expect(TokenId::OpenBracket);
        let index = self.parse_expr(BindingPower::Default);
        self.advance_expect(TokenId::CloseBracket);

        Expr::Index {
            base: Box::new(left),
            index: Box::new(index),
        }
    }
    pub fn parse_group_expr(&mut self) -> ParseExprRes {
        self.advance_expect(TokenId::OpenParen);
        let expr = self.parse_expr(BindingPower::Default);
        self.advance_expect(TokenId::CloseParen);
        Expr::Group {
            inner: Box::new(expr),
        }
    }

    pub fn parse_struct_create(&mut self, left: Expr, bp: BindingPower) -> ParseExprRes {
        if left.to_id() != ExprId::Ident {
            blub_compile_error!("struct name should be an identifier");
        }

        self.advance_expect(TokenId::OpenBlock);

        let mut args = Vec::new();

        loop {
            if self.curr_token().to_id() == TokenId::CloseBlock {
                self.advance();
                break;
            }
            let name = match self.advance_expect(TokenId::Ident) {
                Token::Ident(x) => x,
                _ => unreachable!(),
            };

            self.advance_expect(TokenId::Colon);

            let value = self.parse_expr(bp.clone());

            args.push((name, value));

            if self.curr_token().to_id() == TokenId::Comma {
                self.advance();
                // If the next token is CloseBlock, break (trailing comma)
                if self.curr_token().to_id() == TokenId::CloseBlock {
                    self.advance();
                    break;
                }
            } else if self.curr_token().to_id() == TokenId::CloseBlock {
                self.advance();
                break;
            } else {
                blub_compile_error!("expected ',' or '}}' after struct field");
            }
        }
        Expr::StructCreate {
            struct_ident: Box::new(left),
            fields: args,
        }
    }

    pub fn parse_array_init(&mut self) -> ParseExprRes {
        self.advance_expect(TokenId::OpenBracket);

        // Check for empty array: []
        if self.curr_token().to_id() == TokenId::CloseBracket {
            blub_compile_error!("cannot create arrays with no size");
        }

        // Parse the first expression
        let first_expr = self.parse_expr(BindingPower::Default);

        match self.curr_token().to_id() {
            TokenId::SemiColon => {
                // [init_expr; count]
                self.advance();
                let count_expr = self.parse_expr(BindingPower::Default);
                self.advance_expect(TokenId::CloseBracket);
                Expr::ArrayInit {
                    kind: ArrayInitExprKind::DefaultValue {
                        init: Box::new(first_expr),
                        count: Box::new(count_expr),
                    },
                }
            }
            TokenId::Comma => {
                // [item1, item2, ...]
                let mut items = vec![first_expr];
                while self.curr_token().to_id() == TokenId::Comma {
                    self.advance();
                    if self.curr_token().to_id() == TokenId::CloseBracket {
                        break;
                    }
                    items.push(self.parse_expr(BindingPower::Default));
                }
                self.advance_expect(TokenId::CloseBracket);
                Expr::ArrayInit {
                    kind: ArrayInitExprKind::InitItems { items },
                }
            }
            TokenId::CloseBracket => {
                // Single item: [item1]
                self.advance();
                Expr::ArrayInit {
                    kind: ArrayInitExprKind::InitItems {
                        items: vec![first_expr],
                    },
                }
            }
            _ => {
                blub_compile_error!(
                    "expected ';', ',' or ']' in array initializer, got {}",
                    self.curr_token().to_id()
                );
            }
        }
    }
}

/*












*/
