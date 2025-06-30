use regex::Regex;

use crate::blub_ice;

use super::token::{Number, Token};

pub struct LexerHandlers {
    pub handlers: Vec<LexerHandler>,
}
impl LexerHandlers {
    pub fn new() -> Self {
        Self {
            handlers: Vec::new(),
        }
    }

    pub fn set_handlers(&mut self) {
        self.handlers = vec![
            Box::new(|lexer| {
                if lexer.rem_code.is_empty() {
                    lexer.tokens.push(Token::Eof);
                    return true;
                }
                //
                false
            }),
            keyword_handler("return", Token::Return),
            keyword_handler("let", Token::Let),
            keyword_handler("mut", Token::Mut),
            //
            keyword_handler("use", Token::Use),
            keyword_handler("fn", Token::Fn),
            keyword_handler("for", Token::For),
            keyword_handler("in", Token::In),
            keyword_handler("loop", Token::Loop),
            keyword_handler("match", Token::Match),
            keyword_handler("if", Token::If),
            keyword_handler("struct", Token::Struct),
            keyword_handler("pub", Token::Pub),
            //
            keyword_handler("or", Token::Or),
            keyword_handler("and", Token::And),
            //
            //
            default_handler("@", Token::At),
            default_handler("&", Token::And),
            default_handler("\\(", Token::OpenParen),
            default_handler("\\)", Token::CloseParen),
            default_handler("\\[", Token::OpenBracket),
            default_handler("\\]", Token::CloseBracket),
            default_handler("\\{", Token::OpenBlock),
            default_handler("\\}", Token::CloseBlock),
            //
            default_handler("==", Token::Eq),
            default_handler("=", Token::Assignment),
            default_handler("!", Token::Not),
            default_handler("!=", Token::NotEq),
            default_handler("<=", Token::LessEq),
            default_handler(">=", Token::GreaterEq),
            default_handler("<", Token::Less),
            default_handler(">", Token::Greater),
            //
            default_handler("\\.\\.=", Token::RangeIncl),
            default_handler("\\.\\.", Token::Range),
            default_handler("\\.", Token::Dot),
            default_handler(";", Token::SemiColon),
            default_handler(":", Token::Colon),
            default_handler(",", Token::Comma),
            default_handler("->", Token::Arrow),
            //
            default_handler("\\+=", Token::PlusEq),
            default_handler("-=", Token::MinusEq),
            //
            default_handler("\\+", Token::Plus),
            default_handler("-", Token::Minus),
            multi_line_comment_handler(),
            comment_handler(),
            default_handler("/", Token::Div),
            default_handler("\\*", Token::Star),
            default_handler("%", Token::Percent),
            //
            //
            string_handler(),
            ident_handler(),
            number_handler(),
        ];
    }
}

fn multi_line_comment_handler() -> LexerHandler {
    Box::new(|lexer| {
        let res = match Regex::new(r"/\*(?s)(.*?)\*/").unwrap().find(lexer.rem_code) {
            Some(x) => x,
            None => return false,
        };

        if res.start() != 0 {
            return false;
        }
        lexer.set_head(lexer.head + res.end());
        lexer.handle_whitespace();
        true
    })
}

fn comment_handler() -> LexerHandler {
    Box::new(|lexer| {
        let res = match Regex::new(r"//.*").unwrap().find(lexer.rem_code) {
            Some(x) => x,
            None => return false,
        };

        if res.start() != 0 {
            return false;
        }
        lexer.set_head(lexer.head + res.end());
        lexer.handle_whitespace();
        true
    })
}

impl Default for LexerHandlers {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Lexer<'a> {
    code: &'a str,
    pub rem_code: &'a str,
    pub head: usize,
    pub tokens: Vec<Token>,
    handlers: &'a LexerHandlers,
}
impl<'a> Lexer<'a> {
    pub fn new(code: &'a str, handlers: &'a LexerHandlers) -> Self {
        Self {
            rem_code: code,
            code,
            tokens: Vec::new(),
            head: 0,
            handlers,
        }
    }
    pub fn set_head(&mut self, pos: usize) {
        self.head = pos;
        self.rem_code = &self.code[pos..]
    }
    pub fn handle_whitespace(&mut self) {
        let pattern = Regex::new(r"[\s]*").unwrap();
        let res = pattern
            .find_at(self.rem_code, 0)
            .expect("expected whitespace after a token");
        self.set_head(self.head + res.end());
    }
    pub fn step(&mut self) {
        let mut handled = false;
        for handler in self.handlers.handlers.iter() {
            if handler(self) {
                handled = true;
                break;
            }
        }
        if !handled {
            blub_ice!(
                "there was no handler for token at position {}, and remaining:\n{}",
                self.head,
                self.rem_code
            );
        }
    }
    pub fn tokenize(&mut self) {
        self.tokens = Vec::new();
        self.head = 0;
        self.rem_code = self.code;
        while !self.rem_code.is_empty() {
            self.step()
        }
        self.tokens.push(Token::Eof);
    }
}
type LexerHandler = Box<dyn Fn(&mut Lexer) -> bool>;

fn keyword_handler(keyword: &'static str, token: Token) -> LexerHandler {
    Box::new(move |lexer| {
        let res = match Regex::new(format!(r"{}\s", keyword).as_str())
            .unwrap()
            .find(lexer.rem_code)
        {
            Some(x) => x,
            None => return false,
        };
        if res.start() != 0 {
            return false;
        }
        lexer.set_head(lexer.head + res.end());
        lexer.handle_whitespace();
        lexer.tokens.push(token.clone());
        true
    })
}
fn string_handler() -> LexerHandler {
    Box::new(|lexer| {
        let res = Regex::new(r#""(.*?)""#).unwrap().captures(lexer.rem_code);
        if match &res {
            Some(x) => x.get(0).unwrap().start() != 0,
            None => true,
        } {
            return false;
        }
        let capture = res.as_ref().unwrap().get(1);
        if capture.is_none() {
            return false;
        }
        lexer.set_head(lexer.head + res.unwrap().get(0).unwrap().end());
        lexer.handle_whitespace();
        lexer
            .tokens
            .push(Token::String(capture.unwrap().as_str().into()));
        true
    })
}
fn ident_handler() -> LexerHandler {
    Box::new(|lexer| {
        let res = Regex::new("[a-zA-Z_][a-zA-Z0-9_]*")
            .unwrap()
            .find_at(lexer.rem_code, 0);
        if match res {
            Some(x) => x.start() != 0,
            None => true,
        } {
            return false;
        }
        lexer.set_head(lexer.head + res.unwrap().end());
        lexer.handle_whitespace();
        lexer
            .tokens
            .push(Token::Ident(res.unwrap().as_str().into()));
        true
    })
}
fn number_handler() -> LexerHandler {
    Box::new(|lexer| {
        //

        let res = Regex::new(r"(?:-)?[0-9]+(?:\.[0-9]+)?")
            .unwrap()
            .find_at(lexer.rem_code, 0);
        if match res {
            Some(x) => x.start() != 0,
            None => true,
        } {
            return false;
        }
        let is_neg = Regex::new("-")
            .unwrap()
            .find_at(res.unwrap().as_str(), 0)
            .is_some();

        let whole_seg = Regex::new("[0-9]+").unwrap().find(res.unwrap().as_str());
        if let Some(dec_part) = Regex::new(r"\.([0-9]+)").unwrap().captures(lexer.rem_code) {
            lexer.tokens.push(Token::Number(Number {
                is_neg,
                whole_seg: whole_seg.unwrap().as_str().to_owned(),
                dec_seg: dec_part.get(1).unwrap().as_str().to_owned(),
                number_type: "".into(),
            }));
        } else {
            lexer.tokens.push(Token::Number(Number {
                is_neg,
                whole_seg: whole_seg.unwrap().as_str().to_owned(),
                dec_seg: "".into(),
                number_type: "".into(),
            }));
        }

        lexer.set_head(lexer.head + res.unwrap().end());
        lexer.handle_whitespace();

        true
    })
}

fn default_handler(pattern: &'static str, token: Token) -> LexerHandler {
    Box::new(move |lexer| {
        let res = match Regex::new(pattern).unwrap().find(lexer.rem_code) {
            Some(x) => x,
            None => return false,
        };

        if res.start() != 0 {
            return false;
        }
        lexer.set_head(lexer.head + res.end());
        lexer.handle_whitespace();
        lexer.tokens.push(token.clone());
        true
    })
}
