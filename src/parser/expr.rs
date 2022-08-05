use super::Parser;
use crate::ast::{Expr, ExprKind, Identifier, Stmt};
use crate::token::*;

fn get_unary_operator_precedence(token: &Token) -> i32 {
    use PunctuatorKind::*;
    use TokenKind::*;
    match token.kind {
        Punctuator(Plus) | Punctuator(Minus) | Punctuator(Exclamation) => 6,
        _ => 0,
    }
}
fn get_binary_operator_precedence(token: &Token) -> i32 {
    use PunctuatorKind::*;
    use TokenKind::*;
    match token.kind {
        Punctuator(Asterisk) | Punctuator(Slash) | Punctuator(Percent) => 5,
        Punctuator(Plus) | Punctuator(Minus) => 4,
        Punctuator(Less)
        | Punctuator(LessEqual)
        | Punctuator(Greater)
        | Punctuator(GreaterEqual)
        | Punctuator(EqualEqual)
        | Punctuator(BangEqual) => 3,
        Punctuator(Ampersand) | Punctuator(AmpersandAmpersand) => 2,
        Punctuator(VerticalBar) | Punctuator(PipePipe) => 1,
        _ => 0,
    }
}
#[derive(Clone, Copy)]
enum Associativity {
    Left,
    Right,
    None,
}
#[derive(Clone)]
pub struct BindingPower {
    left: f32,
    right: f32,
}
impl BindingPower {
    fn new(precedence: u32, associativity: Associativity) -> Self {
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
    // fn is_zero(&self) -> bool {
    //     self.left == 0.0 && self.right == 0.0
    // }
}

pub fn set_infix_binding_power() -> std::collections::HashMap<String, BindingPower> {
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
        .map(|(tk, prec, asso)| (stringify(tk), BindingPower::new(*prec, *asso)))
        .collect::<HashMap<String, BindingPower>>()
}

fn get_infix_binding_power(token: &Token, p: &Parser) -> Option<BindingPower> {
    // // Workaround, so that we don't have to implement stuff only for the HashMap.
    fn stringify(tk: &TokenKind) -> String {
        format!("{:?}", tk)
    }

    p.infix_bp.get(&stringify(&token.kind)).cloned()
}

/// This impl block is for parsing expressions
impl Parser {
    pub fn parse_expr(&mut self) -> Box<Expr> {
        self.parse_expr_binary(0.0)
    }
    fn parse_expr_if(&mut self) -> Box<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        self.eat(&Keyword(If)).expect("Expected 'if'");
        let condition = self.parse_expr();
        self.check(&Punctuator(LeftBrace), 0)
            .expect("Expected '{' after if condition");
        let then_branch = self.parse_expr_block();
        self.eat(&Punctuator(RightBrace))
            .expect("Expected '}' after if body");

        let else_branch = match self.peek(0).unwrap().kind {
            Keyword(Else) => Some(self.parse_expr_else()),
            _ => None,
        };
        Box::from(Expr {
            kind: ExprKind::If(condition, then_branch, else_branch),
        })
    }
    fn parse_expr_else(&mut self) -> Box<Expr> {
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
                self.eat(&Punctuator(RightBrace))
                    .expect("Expected '}' after else body");
                e
            }
        }
    }
    fn parse_expr_while(&mut self) -> Box<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;
        self.eat(&Keyword(While)).unwrap();
        let condition = self.parse_expr();
        self.check(&Punctuator(LeftBrace), 0)
            .expect("Expect '{' after while condition.");
        let while_body = self.parse_expr_block();
        self.eat(&Punctuator(RightBrace))
            .expect("Expect '}' after while body.");
        Box::from(Expr {
            kind: ExprKind::While(condition, while_body),
        })
    }
    fn parse_expr_for(&mut self) -> Box<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        self.eat(&Keyword(For)).unwrap();
        let initilizer = match self.peek(0).unwrap().kind {
            Keyword(Var) => Some(Box::from(*self.parse_stmt_var())),
            Punctuator(Semicolon) => {
                self.advance();
                None
            }
            _ => Some(self.parse_stmt_expression()),
        };

        let condition = self.parse_expr();
        self.eat(&Punctuator(Semicolon))
            .expect("Expected ';' after for condition");

        let itr_expr = self.parse_expr();
        self.check(&Punctuator(LeftBrace), 0)
            .expect("Expected '{' after for iterator expression");

        let while_body = self.parse_expr_block();
        self.eat(&Punctuator(RightBrace))
            .expect("Expected '}' after for body");
        Box::from(Expr {
            kind: ExprKind::For(initilizer.unwrap(), condition, itr_expr, while_body),
        })
    }
    pub fn parse_expr_block(&mut self) -> Box<Expr> {
        use PunctuatorKind::*;
        use TokenKind::*;

        self.eat(&Punctuator(LeftBrace)).unwrap();
        let mut statements: Vec<Stmt> = Vec::new();

        while !self.is_at_end() && self.check(&Punctuator(RightBrace), 0).is_err() {
            statements.push(*self.parse_stmt());
        }

        Box::from(Expr {
            kind: ExprKind::Block(statements),
        })
    }
    fn parse_expr_binary(&mut self, prev_bp: f32) -> Box<Expr> {
        let un_op_prec = get_unary_operator_precedence(self.peek(0).unwrap());
        let mut left = if un_op_prec != 0 && un_op_prec >= prev_bp as i32 {
            let op = self.eat_punctuator().unwrap().clone();
            let right = self.parse_expr_binary(un_op_prec as f32);
            Box::from(Expr {
                kind: ExprKind::Unary(op, right),
            })
        } else {
            self.parse_expr_primary()
        };

        loop {
            match get_infix_binding_power(self.peek(0).unwrap(), &self) {
                None => break,
                Some(bp) if bp.left < prev_bp => break,
                Some(bp) if bp.left == prev_bp => panic!(
                    "Operators have the same precedence and are non-associative. This is not {:?}",
                    self.peek(0)
                ),
                Some(bp) => {
                    let operator = self.eat_punctuator().unwrap().clone();
                    let right = self.parse_expr_binary(bp.right);
                    left = Box::from(Expr {
                        kind: ExprKind::Binary(left, operator.clone(), right),
                    });
                }
            }
        }
        left
    }
    fn parse_expr_primary(&mut self) -> Box<Expr> {
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
                    *self.parse_expr_fn_call()
                } else {
                    let mut ids = Vec::new();
                    ids.push(self.eat_identifier().unwrap().clone());

                    while self.eat(&TokenKind::Punctuator(ColonColon)).is_ok() {
                        ids.push(self.eat_identifier().unwrap().clone());
                    }

                    let actual_id = Identifier::from_token(ids.pop().unwrap());
                    let actual_path = if ids.is_empty() {
                        None
                    } else {
                        Some(
                            ids.iter()
                                .map(|x| Identifier::from_token(x.clone()))
                                .collect::<Vec<_>>(),
                        )
                    };

                    Expr {
                        kind: ExprKind::Symbol {
                            name: actual_id,
                            path: actual_path,
                        },
                    }
                }
            }

            TokenKind::Keyword(If) => *self.parse_expr_if(),
            TokenKind::Keyword(While) => *self.parse_expr_while(),
            TokenKind::Keyword(For) => *self.parse_expr_for(),
            TokenKind::Punctuator(LeftParen) => {
                self.advance(); //skip the (
                let left = self.parse_expr();
                self.eat(&TokenKind::Punctuator(RightParen))
                    .expect("Expected ')' after expression");
                Expr {
                    kind: ExprKind::Grouping(left),
                }
            }
            TokenKind::Punctuator(LeftBrace) => {
                let stmts = self.parse_expr_block();
                self.eat(&TokenKind::Punctuator(RightBrace))
                    .expect("Expected '}' after expression");
                *stmts
            }
            unhandled => {
                panic!("{:?}", &unhandled)
            }
        };
        Box::from(expr)
    }
    fn parse_expr_fn_call(&mut self) -> Box<Expr> {
        use PunctuatorKind::*;
        //use TokenKind::*;

        let callee_name = self.eat_identifier().cloned().unwrap();
        let callee_name = Identifier::from_token(callee_name);

        let callee = Box::from(Expr {
            kind: ExprKind::Symbol {
                name: callee_name,
                path: None,
            },
        });
        self.eat(&TokenKind::Punctuator(LeftParen))
            .expect("Expected '(' after function name");

        let mut arguments: Vec<Expr> = Vec::new();
        while self.peek(0).unwrap().kind != TokenKind::Punctuator(RightParen) {
            arguments.push(*self.parse_expr());
            if self.peek(0).unwrap().kind != TokenKind::Punctuator(RightParen) {
                self.eat(&TokenKind::Punctuator(Comma))
                    .expect("Expected ',' after argument");
            }
        }

        self.eat(&TokenKind::Punctuator(RightParen))
            .expect("Expected ')' after arguments");

        Box::from(Expr {
            kind: ExprKind::FnCall(callee, arguments),
        })
    }
}
