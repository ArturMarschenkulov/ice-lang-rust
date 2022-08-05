use super::Parser;
use crate::ast::{ExprKind, Stmt, StmtKind};
use crate::token::*;

/// This impl block is for parsing statements
impl Parser {
    pub fn parse_stmt(&mut self) -> Stmt {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        let token = self.peek(0);
        let sk = match &token.unwrap().kind {
            Keyword(Var) => self.parse_stmt_var(),
            s if s.starts_item() => Stmt {
                kind: StmtKind::Item(self.parse_item()),
            },
            Punctuator(Semicolon) => {
                self.advance();
                Stmt {
                    kind: StmtKind::NoOperation,
                }
            }
            _ => self.parse_stmt_expression(),
        };
        match sk.kind {
            StmtKind::Var { .. } => {
                let _ = self.eat(&Punctuator(Semicolon));
            }
            StmtKind::Expression(ref expr) => {
                match expr.clone().kind {
                    ExprKind::Block(..)
                    | ExprKind::If(..)
                    | ExprKind::While(..)
                    | ExprKind::For(..) => {
                        // self.eat_tok(&Punctuator(Semicolon))
                        //     .expect("Expected ';' after expression");
                    }
                    _ => {
                        self.eat(&Punctuator(Semicolon))
                            .expect("Expected ';' after expression");
                    }
                }
            }
            _ => (),
        }
        sk
    }
    pub fn parse_stmt_expression(&mut self) -> Stmt {
        use PunctuatorKind::*;
        use TokenKind::*;
        let expr = self.parse_expr();

        let s = match expr.kind {
            ExprKind::Block(..) | ExprKind::If(..) | ExprKind::While(..) | ExprKind::For(..) => {
                StmtKind::Expression(Box::new(expr))
            }
            _ => {
                let stmt = if self.eat(&Punctuator(Semicolon)).is_ok() {
                    StmtKind::Expression(Box::new(expr))
                } else {
                    StmtKind::ExpressionWithoutSemicolon(Box::new(expr))
                };
                stmt
            }
        };

        Stmt { kind: s }
    }
}
