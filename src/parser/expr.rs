use crate::ast::{Expr, ExprKind, Identifier, Operator, Stmt};
use crate::token::{KeywordKind, PunctuatorKind, TokenKind};

use super::{PResult, Parser};

#[derive(Clone, Copy)]
enum Associativity {
    Left,
    Right,
    None,
}

pub struct BindingPower2 {
    associativity: Associativity,
    precedence: u32,
}
impl BindingPower2 {
    fn new() -> Self {
        Self {
            associativity: Associativity::None,
            precedence: 0,
        }
    }
    fn from(precedence: u32, associativity: Associativity) -> Self {
        Self {
            associativity,
            precedence,
        }
    }
    fn is_zero(&self) -> bool {
        self.precedence == 0
    }
}

/// `BindingPower` represents the binding power of an operator. It combines both precedence and associativity.
///
/// Unlike traditional precedence where the smaller the number the more tightly it binds, here the bigger the number the more tightly it binds.
#[derive(Clone)]
pub struct BindingPower {
    left: f32,
    right: f32,
}
impl BindingPower {
    fn new() -> Self {
        Self {
            left: 0.0,
            right: 0.0,
        }
    }
    fn from(precedence: u32, associativity: Associativity) -> Self {
        use Associativity::*;
        let prec = precedence as f32;
        match associativity {
            Left => BindingPower {
                left: prec + 0.1,
                right: prec - 0.1,
            },
            Right => BindingPower {
                left: prec - 0.1,
                right: prec + 0.1,
            },
            None => BindingPower {
                left: prec,
                right: prec,
            },
        }
    }
}

/// This is for the default binding powers impl. This is intended to be temporary, as in the future the user will be able to change the binding powers
/// and the default binding powers will be in a standard library.
impl BindingPower {
    pub fn infix_binding_powers() -> std::collections::HashMap<String, BindingPower> {
        use std::collections::HashMap;
        use Associativity::*;
        use PunctuatorKind::*;
        use TokenKind::*;
        let arr = [
            //
            (Punctuator(Asterisk), 7, Left),
            (Punctuator(Slash), 7, Left),
            (Punctuator(Percent), 7, Left),
            //
            (Punctuator(Plus), 6, Left),
            (Punctuator(Minus), 6, Left),
            //
            (Punctuator(Less), 4, None),
            (Punctuator(LessEqual), 4, None),
            (Punctuator(Greater), 3, None),
            (Punctuator(GreaterEqual), 4, None),
            (Punctuator(EqualEqual), 4, None),
            (Punctuator(BangEqual), 4, None),
            //
            (Punctuator(Ampersand), 3, None),
            (Punctuator(VerticalBar), 2, None),
            //
            (Punctuator(Equal), 1, Right),
        ];
        // Workaround, so that we don't have to implement stuff only for the HashMap.
        fn stringify(tk: &TokenKind) -> String {
            format!("{:?}", tk)
        }
        arr.iter()
            .map(|(tk, prec, asso)| (stringify(tk), BindingPower::from(*prec, *asso)))
            .collect::<HashMap<String, BindingPower>>()
    }
    pub fn postfix_binding_powers() -> std::collections::HashMap<String, BindingPower> {
        use std::collections::HashMap;
        fn stringify(tk: &TokenKind) -> String {
            format!("{:?}", tk)
        }
        let arr: [(TokenKind, u32, Associativity); 0] = [];
        arr.iter()
            .map(|(tk, prec, asso)| (stringify(tk), BindingPower::from(*prec, *asso)))
            .collect::<HashMap<String, BindingPower>>()
    }
    pub fn prefix_binding_powers() -> std::collections::HashMap<String, BindingPower> {
        use std::collections::HashMap;
        use Associativity::*;
        use PunctuatorKind::*;
        use TokenKind::*;
        fn stringify(tk: &TokenKind) -> String {
            format!("{:?}", tk)
        }
        let arr = [(Punctuator(Plus), 6, Right), (Punctuator(Minus), 6, Right)];
        arr.iter()
            .map(|(tk, prec, asso)| (stringify(tk), BindingPower::from(*prec, *asso)))
            .collect::<HashMap<String, BindingPower>>()
    }
}

fn infix_binding_power(p: &Parser, token: &Token) -> Option<BindingPower> {
    fn stringify(tk: &TokenKind) -> String {
        format!("{:?}", tk)
    }
    p.infix_bp.get(&stringify(&token.kind)).cloned()
}
fn postfix_binding_power(_p: &Parser, token: &Token) -> Option<BindingPower> {
    fn stringify(tk: &TokenKind) -> String {
        format!("{:?}", tk)
    }
    BindingPower::postfix_binding_powers()
        .get(&stringify(&token.kind))
        .cloned()
}
fn prefix_binding_power(_p: &Parser, token: &Token) -> Option<BindingPower> {
    fn stringify(tk: &TokenKind) -> String {
        format!("{:?}", tk)
    }
    BindingPower::prefix_binding_powers()
        .get(&stringify(&token.kind))
        .cloned()
}

/// This impl block is for parsing expressions
impl Parser {
    pub fn parse_expr(&mut self) -> PResult<Expr> {
        self.parse_expr_binary(BindingPower::new())
    }
}

/// This impl block is for parsing control flow expressions
impl Parser {
    fn parse_expr_if(&mut self) -> PResult<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        self.eat(&Keyword(If)).expect("Expected 'if'");
        let condition = self.parse_expr().unwrap();
        self.check(&Punctuator(LeftBrace), 0)
            .expect("Expected '{' after if condition");
        let then_branch = self.parse_expr_block().unwrap();
        self.check(&Punctuator(RightBrace), -1)
            .expect("Expected '}' after if body");

        let else_branch = match self.peek(0).unwrap().kind {
            Keyword(Else) => Some(Box::new(self.parse_expr_else().unwrap())),
            _ => None,
        };
        let expr = Expr {
            kind: ExprKind::If(Box::new(condition), Box::new(then_branch), else_branch),
        };
        Ok(expr)
    }
    fn parse_expr_else(&mut self) -> PResult<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;
        self.eat(&Keyword(Else)).expect("Expected 'else'");
        match self.peek(0).unwrap().kind {
            Keyword(If) => {
                //self.advance();
                self.parse_expr_if()
            }
            _ => {
                self.check(&Punctuator(LeftBrace), 0)
                    .expect("Expected '{' after else");
                let e = self.parse_expr_block();
                self.check(&Punctuator(RightBrace), -1)
                    .expect("Expected '}' after else body");
                e
            }
        }
    }
    fn parse_expr_while(&mut self) -> PResult<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;
        self.eat(&Keyword(While)).unwrap();
        let condition = self.parse_expr().unwrap();
        self.check(&Punctuator(LeftBrace), 0)
            .expect("Expect '{' after while condition.");
        let while_body = self.parse_expr_block().unwrap();
        self.check(&Punctuator(RightBrace), -1)
            .expect("Expect '}' after while body.");
        let expr = Expr {
            kind: ExprKind::While(Box::new(condition), Box::new(while_body)),
        };
        Ok(expr)
    }
    fn parse_expr_for(&mut self) -> PResult<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        self.eat(&Keyword(For)).unwrap();
        let initilizer = match self.peek(0).unwrap().kind {
            Keyword(Var) => Some(self.parse_stmt_var().unwrap()),
            Punctuator(Semicolon) => {
                self.advance();
                None
            }
            _ => Some(self.parse_stmt_expression().unwrap()),
        };

        let condition = self.parse_expr().unwrap();
        self.eat(&Punctuator(Semicolon))
            .expect("Expected ';' after for condition");

        let itr_expr = self.parse_expr().unwrap();
        self.check(&Punctuator(LeftBrace), 0)
            .expect("Expected '{' after for iterator expression");

        let while_body = self.parse_expr_block().unwrap();
        self.check(&Punctuator(RightBrace), -1)
            .expect("Expected '}' after for body");
        let expr = Expr {
            kind: ExprKind::For(
                Box::new(initilizer.unwrap()),
                Box::new(condition),
                Box::new(itr_expr),
                Box::new(while_body),
            ),
        };
        Ok(expr)
    }
}
impl Parser {
    pub fn parse_expr_block(&mut self) -> PResult<Expr> {
        use PunctuatorKind::*;
        use TokenKind::*;

        self.eat(&Punctuator(LeftBrace)).unwrap();
        let mut statements: Vec<Stmt> = Vec::new();

        while !self.is_at_end() && self.check(&Punctuator(RightBrace), 0).is_err() {
            statements.push(self.parse_stmt().unwrap());
        }

        self.eat(&Punctuator(RightBrace)).unwrap();

        let expr = Expr {
            kind: ExprKind::Block(statements),
        };
        Ok(expr)
    }
    /// Parses a binary or unary expression.
    ///
    /// This function is recursive, and it uses Pratt parsing. This function is based on [this article](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html).
    ///
    /// This function is also used as an entry point for parsing expressions. In that case the `prev_bp` is `BindingPower::new()`.
    fn parse_expr_binary(&mut self, prev_bp: BindingPower) -> PResult<Expr> {
        let un_prefix = self.peek(0).unwrap();
        let mut left = match prefix_binding_power(self, un_prefix) {
            Some(bp) if bp.left >= prev_bp.right => {
                let op = self.eat_punctuator().unwrap().clone();
                let right = self.parse_expr_binary(bp).unwrap();
                Expr {
                    kind: ExprKind::UnaryPrefix(op, Box::new(right)),
                }
            }
            Some(_) | None => self.parse_expr_primary().unwrap(),
        };

        loop {
            let bin_infix = self.peek(0).unwrap();
            match infix_binding_power(self, bin_infix) {
                // This is not a binary infix operator.
                None => break,
                // This is a binary infix operator, but it binds less tightly than the previous operator.
                Some(bp) if bp.left < prev_bp.right => break,
                // This is a binary infix operator, but it binds equally tightly as the previous operator.
                Some(bp) if bp.left == prev_bp.right => panic!(
                    "Operators have the same precedence and are non-associative. This is not {:?}",
                    self.peek(0)
                ),
                // This is a binary infix operator, but it binds more tightly than the previous operator.
                Some(bp) => {
                    let operator = self.eat_punctuator().unwrap().clone();
                    let right = self.parse_expr_binary(bp).unwrap();
                    left = Expr {
                        kind: ExprKind::BinaryInfix(
                            Box::new(left),
                            operator.clone(),
                            Box::new(right),
                        ),
                    };
                }
            }
        }
        Ok(left)
    }

    pub fn parse_identifier(&mut self) -> PResult<Identifier> {
        let token = match self.eat_identifier() {
            Ok(token) => token.clone(),
            Err(token) => panic!("Expected identifier, got {:?}", token),
        };
        let identifier = Identifier::from(token);
        Ok(identifier)
    }
    fn parse_symbol(&mut self) -> PResult<Expr> {
        use PunctuatorKind::*;
        let mut ids = Vec::new();
        ids.push(self.parse_identifier().unwrap());

        while self.eat(&TokenKind::Punctuator(ColonColon)).is_ok() {
            ids.push(self.parse_identifier().unwrap());
        }

        let actual_id = ids.pop().expect("Expected identifier");
        let actual_path = if ids.is_empty() { None } else { Some(ids) };

        let expr = Expr {
            kind: ExprKind::Symbol {
                name: actual_id,
                path: actual_path,
            },
        };
        Ok(expr)
    }
    fn parse_expr_primary(&mut self) -> PResult<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        // use TokenKind::*;
        let token = self.peek(0).unwrap().clone();

        let expr = match token.kind {
            TokenKind::Literal(lit) => {
                self.advance();
                Expr {
                    kind: ExprKind::Literal(lit),
                }
            }
            TokenKind::Identifier(_) => {
                if self.check(&TokenKind::Punctuator(LeftParen), 1).is_ok() {
                    self.parse_expr_fn_call().unwrap()
                } else {
                    self.parse_symbol().unwrap()
                }
            }

            TokenKind::Keyword(If) => self.parse_expr_if().unwrap(),
            TokenKind::Keyword(While) => self.parse_expr_while().unwrap(),
            TokenKind::Keyword(For) => self.parse_expr_for().unwrap(),
            TokenKind::Punctuator(LeftParen) => {
                self.eat(&TokenKind::Punctuator(LeftParen))
                    .expect("Expected '(' before expression");
                let left = self.parse_expr().unwrap();
                self.eat(&TokenKind::Punctuator(RightParen))
                    .expect("Expected ')' after expression");
                Expr {
                    kind: ExprKind::Grouping(Box::new(left)),
                }
            }
            TokenKind::Punctuator(LeftBrace) => {
                let stmts = self.parse_expr_block().unwrap();
                self.check(&TokenKind::Punctuator(RightBrace), -1)
                    .expect("Expected '}' after expression");
                stmts
            }
            unhandled => {
                panic!("{:?}", &unhandled)
            }
        };
        Ok(expr)
    }
    fn parse_expr_fn_call(&mut self) -> PResult<Expr> {
        use PunctuatorKind::*;

        let callee_name = self.parse_identifier().unwrap();

        let callee = Box::from(Expr {
            kind: ExprKind::Symbol {
                name: callee_name,
                path: None,
            },
        });
        self.eat(&TokenKind::Punctuator(LeftParen))
            .expect("Expected '(' after function name");

        let mut arguments: Vec<Expr> = Vec::new();
        while self.check(&TokenKind::Punctuator(RightParen), 0).is_err() {
            arguments.push(self.parse_expr().unwrap());
            if self.check(&TokenKind::Punctuator(RightParen), 0).is_ok() {
                self.eat(&TokenKind::Punctuator(Comma))
                    .expect("Expected ',' after argument");
            }
        }

        self.eat(&TokenKind::Punctuator(RightParen))
            .expect("Expected ')' after arguments");

        let expr = Expr {
            kind: ExprKind::FnCall(callee, arguments),
        };
        Ok(expr)
    }
}
