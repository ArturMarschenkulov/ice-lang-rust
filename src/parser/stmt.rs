use super::super::lexer::token::{KeywordKind, PunctuatorKind, TokenKind};
use super::ast::{Stmt, StmtKind};

use super::{PResult, Parser};

/// This impl block is for parsing statements
impl Parser {
    pub fn parse_stmt(&mut self) -> PResult<Stmt> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        let token = self.peek(0);
        let sk = match &token.unwrap().kind {
            Keyword(Var) => self.parse_stmt_var().unwrap(),
            s if s.starts_item() => Stmt::item(self.parse_item().unwrap()),
            Punctuator(Semicolon) => {
                self.advance();
                Stmt::noop()
            }
            _ => self.parse_stmt_expression().unwrap(),
        };
        match sk.kind {
            StmtKind::Var { .. } => {
                let _ = self.eat(&Punctuator(Semicolon));
            }
            StmtKind::Expression(ref expr) => {
                match !expr.does_require_semicolon_if_in_stmt() {
                    true => {
                        // self.eat_tok(&Punctuator(Semicolon))
                        //     .expect("Expected ';' after expression");
                    }
                    false => {
                        self.eat(&Punctuator(Semicolon))
                            .expect("Expected ';' after expression");
                    }
                }
            }
            _ => (),
        }
        Ok(sk)
    }

    /// Parses a statement which consists of an expression.
    /// 
    /// The main feature is that it does not declare anything.
    /// 
    /// This includes { ... } blocks, however also if, while, for, expressions, etc or simply stuff like `1 + 1;`
    pub fn parse_stmt_expression(&mut self) -> PResult<Stmt> {
        use PunctuatorKind::*;
        use TokenKind::*;
        let expr = self.parse_expr().unwrap();

        let stmt = match !expr.does_require_semicolon_if_in_stmt() {
            true => Stmt::expr(expr),
            false => {
                if self.eat(&Punctuator(Semicolon)).is_ok() {
                    Stmt::expr(expr)
                } else {
                    Stmt::expr_without_semicolon(expr)
                }
            }
        };
        Ok(stmt)
    }
}
