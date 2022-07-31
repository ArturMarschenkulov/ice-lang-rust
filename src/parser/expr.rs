use crate::ast::{Expr, ExprKind, Stmt};
use crate::parser::Parser;
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
struct BindingPower {
    left: f32,
    right: f32,
}
impl BindingPower {
    fn new(precedence: u32, associativity: Associativity) -> Self {
        let prec = precedence as f32;
        match associativity {
            Associativity::Left => BindingPower {
                left: prec + 0.1,
                right: prec - 0.1,
            },
            Associativity::Right => BindingPower {
                left: prec - 0.1,
                right: prec + 0.1,
            },
            Associativity::None => BindingPower {
                left: prec,
                right: prec,
            },
        }
    }
    fn is_zero(&self) -> bool {
        self.left == 0.0 && self.right == 0.0
    }
}
fn get_infix_binding_power(token: &Token) -> BindingPower {
    use std::collections::HashMap;
    use Associativity::*;
    use PunctuatorKind::*;
    use TokenKind::*;

    // Right now this is inefficeint. But the point is to slowly extract that logic to a more global scope
    // This location for this logic is temporary anyway, since custom operators will be supported,
    // meaning that the one has to collect the map for that in the typechecker anyway.

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

    // let s = |tk: &TokenKind| format!("{:?}", tk);
    // Workaround, so that we don't have to implement stuff only for the HashMap.
    fn s(tk: &TokenKind) -> String {
        format!("{:?}", tk)
    }
    arr.iter()
        .map(|(tk, prec, asso)| (s(tk), BindingPower::new(*prec, *asso)))
        .collect::<HashMap<String, BindingPower>>()
        .get(&s(&token.kind))
        .unwrap_or(&BindingPower::new(0, None))
        .clone()
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
            let op = self.peek(0).unwrap().clone();
            self.advance();
            let right = self.parse_expr_binary(un_op_prec as f32);
            Box::from(Expr {
                kind: ExprKind::Unary(op, right),
            })
        } else {
            self.parse_expr_primary()
        };

        loop {
            let binding_power = get_infix_binding_power(self.peek(0).unwrap());
            if binding_power.is_zero() || binding_power.left < prev_bp {
                break;
            } else if binding_power.left == prev_bp {
                // TODO: This is an error. The operators have the same precedence and are non-associative.
                //       We should probably handle this.
                panic!(
                    "Operators have the same precedence and are non-associative. This is not {:?}",
                    self.peek(0)
                );
            }

            let operator = self.peek(0).unwrap().clone();
            self.advance();
            let right = self.parse_expr_binary(binding_power.right);
            left = Box::from(Expr {
                kind: ExprKind::Binary(left, operator.clone(), right),
            });
        }
        left
    }
    fn parse_expr_primary(&mut self) -> Box<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;
        let token = self.peek(0).unwrap().clone();

        let expr = match token.kind {
            Literal(lit) => {
                self.advance();
                Expr {
                    kind: ExprKind::Literal(lit),
                }
            }
            Identifier(_) => {
                if self.check(&Punctuator(LeftParen), 1).is_ok() {
                    *self.parse_expr_fn_call()
                } else {
                    let mut ids = Vec::new();
                    let i = self.eat_identifier().unwrap().clone();
                    ids.push(i);

                    while self.eat(&Punctuator(Colon)).is_ok() {
                        self.eat(&Punctuator(Colon)).unwrap();
                        let i = self.eat_identifier().unwrap().clone();
                        ids.push(i);
                    }

                    let actual_id = ids.pop().unwrap();
                    let actual_path = if ids.is_empty() { None } else { Some(ids) };

                    Expr {
                        kind: ExprKind::Symbol {
                            name: actual_id,
                            path: actual_path,
                        },
                    }
                }
            }

            Keyword(If) => *self.parse_expr_if(),
            Keyword(While) => *self.parse_expr_while(),
            Keyword(For) => *self.parse_expr_for(),
            Punctuator(LeftParen) => {
                self.advance(); //skip the (
                let left = self.parse_expr();
                self.eat(&Punctuator(RightParen))
                    .expect("Expected ')' after expression");
                Expr {
                    kind: ExprKind::Grouping(left),
                }
            }
            Punctuator(LeftBrace) => {
                let stmts = self.parse_expr_block();
                self.eat(&Punctuator(RightBrace))
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
        use TokenKind::*;

        let callee_name = self.eat_identifier().cloned().unwrap();

        let callee = Box::from(Expr {
            kind: ExprKind::Symbol {
                name: callee_name,
                path: None,
            },
        });
        self.eat(&Punctuator(LeftParen))
            .expect("Expected '(' after function name");

        let mut arguments: Vec<Expr> = Vec::new();
        while self.peek(0).unwrap().kind != Punctuator(RightParen) {
            arguments.push(*self.parse_expr());
            if self.peek(0).unwrap().kind != Punctuator(RightParen) {
                self.eat(&Punctuator(Comma))
                    .expect("Expected ',' after argument");
            }
        }

        self.eat(&Punctuator(RightParen))
            .expect("Expected ')' after arguments");

        Box::from(Expr {
            kind: ExprKind::FnCall(callee, arguments),
        })
    }
}
