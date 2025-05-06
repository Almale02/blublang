use crate::{
    blub_compile_error,
    compile::lexer::token::{Token, TokenId},
};

use super::{
    ast::AstType,
    parser::{BindingPower, ParseAstTypeRes, Parser},
};

impl Parser<'_> {
    #[allow(clippy::while_let_loop)]
    pub fn parse_type(&mut self, bp: BindingPower) -> ParseAstTypeRes {
        let token = self.curr_token();
        let nud_handler = match self.handlers.type_nud_lookup.get(&token.to_id()) {
            Some(x) => x,
            None => blub_compile_error!("expected type nud handler for token: {}", token),
        };
        let mut left = nud_handler(self);
        if self.curr_token().to_id() == TokenId::SemiColon {
            return left;
        }
        loop {
            let token_bp = match self
                .handlers
                .type_binding_powers
                .get(&self.curr_token().to_id())
            {
                Some(x) => x.clone(),
                None => break,
            };
            if token_bp < bp {
                break;
            }
            if let Some(led_handler) = self
                .handlers
                .type_led_lookup
                .get(&self.curr_token().to_id())
            {
                left = led_handler(self, left, bp.clone())
            }
        }
        left
    }
    pub fn parse_symbol_type(&mut self) -> ParseAstTypeRes {
        let symbol = match self.advance_expect(TokenId::Ident) {
            Token::Ident(x) => x,
            _ => unreachable!(),
        };
        AstType::Symbol(symbol)
    }
    pub fn parse_array_type(&mut self) -> AstType {
        self.advance_expect(TokenId::OpenBracket);
        self.advance_expect(TokenId::CloseBracket);
        let ast_type = self.parse_type(BindingPower::Default);
        AstType::Array(Box::new(ast_type))
    }
    pub fn parse_ptr_type(&mut self) -> AstType {
        self.advance();
        let is_mut = self.curr_token().to_id() == TokenId::Mut;
        if is_mut {
            self.advance();
        }
        AstType::Pointer {
            is_mut,
            pointee: Box::new(self.parse_type(BindingPower::Default)),
        }
    }
}
/*






















*/
