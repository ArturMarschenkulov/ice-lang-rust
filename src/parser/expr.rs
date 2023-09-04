use super::super::lexer::token::{KeywordKind, PunctuatorKind, TokenKind};
use super::ast::{Expr, Identifier, Operator, Stmt};

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
    fn comp(bp_0: &BindingPower, bp_1: &BindingPower) -> std::cmp::Ordering {
        use std::cmp::Ordering;
        match (bp_0.left, bp_1.right) {
            (l, r) if l > r => Ordering::Greater,
            (l, r) if l < r => Ordering::Less,
            (l, r) if l == r => Ordering::Equal,
            _ => panic!("This should not happen"),
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
        arr.iter()
            .map(|(tk, prec, asso)| (format!("{:?}", tk), BindingPower::from(*prec, *asso)))
            .collect::<HashMap<String, BindingPower>>()
    }
    pub fn postfix_binding_powers() -> std::collections::HashMap<String, BindingPower> {
        use std::collections::HashMap;
        let arr: [(TokenKind, u32, Associativity); 0] = [];
        arr.iter()
            .map(|(tk, prec, asso)| (format!("{:?}", tk), BindingPower::from(*prec, *asso)))
            .collect::<HashMap<String, BindingPower>>()
    }
    pub fn prefix_binding_powers() -> std::collections::HashMap<String, BindingPower> {
        use std::collections::HashMap;
        use Associativity::*;
        use PunctuatorKind::*;
        use TokenKind::*;
        let arr = [(Punctuator(Plus), 6, Right), (Punctuator(Minus), 6, Right)];
        arr.iter()
            .map(|(tk, prec, asso)| (format!("{:?}", tk), BindingPower::from(*prec, *asso)))
            .collect::<HashMap<String, BindingPower>>()
    }
}

fn infix_binding_power(p: &Parser, token: &Operator) -> Option<BindingPower> {
    p.infix_bp.get(&format!("{:?}", &token.name.kind)).cloned()
}
fn postfix_binding_power(_p: &Parser, token: &Operator) -> Option<BindingPower> {
    BindingPower::postfix_binding_powers()
        .get(&format!("{:?}", &token.name.kind))
        .cloned()
}
fn prefix_binding_power(_p: &Parser, token: &Operator) -> Option<BindingPower> {
    BindingPower::prefix_binding_powers()
        .get(&format!("{:?}", token.name.kind))
        .cloned()
}

/// This impl block is for parsing expressions
impl Parser {
    /// Parses an expression.
    /// 
    /// This function is the entry point for parsing expressions.
    pub fn parse_expr(&mut self) -> PResult<Expr> {
        self.parse_expr_binary(BindingPower::new())
    }
}

fn parse_expr_block_(this: &mut Parser, text_0: &str, text_1: &str) -> PResult<Expr> {
    use PunctuatorKind::*;
    use TokenKind::*;

    this.check(&Punctuator(LeftBrace), 0).expect(text_0);
    let block = this.parse_expr_block().unwrap();
    this.check(&Punctuator(RightBrace), -1).expect(text_1);

    Ok(block)
}

/// This impl block is for parsing control flow expressions
impl Parser {
    fn parse_expr_if(&mut self) -> PResult<Expr> {
        use KeywordKind::*;
        use TokenKind::*;

        self.eat(&Keyword(If)).expect("Expected 'if'");
        let condition = self.parse_expr().unwrap();
        let then_branch = parse_expr_block_(
            self,
            "Expected '{' after if condition",
            "Expected '}' after if body",
        )
        .unwrap();

        let else_branch = match self.peek(0).unwrap().kind {
            Keyword(Else) => Some(self.parse_expr_else().unwrap()),
            _ => None,
        };
        Ok(Expr::if_(condition, then_branch, else_branch))
    }
    fn parse_expr_else(&mut self) -> PResult<Expr> {
        use KeywordKind::*;
        use TokenKind::*;
        self.eat(&Keyword(Else)).expect("Expected 'else'");
        match self.peek(0).unwrap().kind {
            Keyword(If) => self.parse_expr_if(),
            _ => parse_expr_block_(
                self,
                "Expected '{' after else",
                "Expected '}' after else body",
            ),
        }
    }
    fn parse_expr_while(&mut self) -> PResult<Expr> {
        use KeywordKind::*;
        use TokenKind::*;
        self.eat(&Keyword(While)).unwrap();
        let condition = self.parse_expr().unwrap();
        let while_body = parse_expr_block_(
            self,
            "Expect '{' after while condition.",
            "Expect '}' after while body.",
        )
        .unwrap();
        Ok(Expr::while_(condition, while_body))
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

        let while_body = parse_expr_block_(
            self,
            "Expected '{' after for iterator expression",
            "Expected '}' after for body",
        )
        .unwrap();

        Ok(Expr::for_(
            initilizer.unwrap(),
            condition,
            itr_expr,
            while_body,
        ))
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
        let expr = Expr::block(statements);
        Ok(expr)
    }
    /// Parses a binary or unary expression.
    ///
    /// This function is recursive, and it uses Pratt parsing. This function is based on [this article](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html).
    ///
    /// This function is also used as an entry point for parsing expressions. In that case the `prev_bp` is `BindingPower::new()`.
    fn parse_expr_binary(&mut self, prev_bp: BindingPower) -> PResult<Expr> {
        let un_prefix = self
            .peek(0)
            .ok_or_else(super::Error::unexpected_eof)?
            .clone();

        let mut left = if un_prefix.kind.is_punctuator() {
            let un_prefix = Operator::try_from(un_prefix).map_err(|x| {
                super::Error::new(format!("Expected unary prefix operator, got {:?}", x))
            })?;

            match prefix_binding_power(self, &un_prefix) {
                Some(bp) if prev_bp.right < bp.left => {
                    let op = self.parse_operator()?;
                    let right = self.parse_expr_binary(bp).unwrap();

                    Expr::unary_prefix(op, right)
                }
                Some(_) | None => self.parse_expr_primary().unwrap(),
            }
        } else {
            self.parse_expr_primary().unwrap()
        };

        loop {
            let bin_infix = self.peek(0).unwrap();
            let bin_infix = Operator::try_from(bin_infix.clone()).unwrap();
            match infix_binding_power(self, &bin_infix) {
                // This is not a binary infix operator.
                None => break,
                // This is a binary infix operator, but it binds less tightly than the previous operator.
                Some(bp) if prev_bp.right > bp.left => break,
                // This is a binary infix operator, but it binds equally tightly as the previous operator.
                Some(bp) if prev_bp.right == bp.left => panic!(
                    "Operators have the same precedence and are non-associative. This is not {:?}",
                    self.peek(0)
                ),
                // This is a binary infix operator, but it binds more tightly than the previous operator.
                Some(bp) => {
                    let operator = self.parse_operator().unwrap();
                    let right = self.parse_expr_binary(bp).unwrap();

                    left = Expr::binary_infix(left, operator, right);
                }
            }
        }
        Ok(left)
    }

    pub fn parse_identifier(&mut self) -> PResult<Identifier> {
        self.eat_identifier()
            .map_err(|token| Error::new(format!("Expected identifier, got {:?}", token)))
            .map(|token| Identifier::try_from(token.clone()).expect("guaranteed"))
    }

    pub fn parse_operator(&mut self) -> PResult<Operator> {
        self.eat_punctuator()
            .map_err(|token| Error::new(format!("Expected operator, got {:?}", token)))
            .map(|token| Operator::try_from(token.clone()).expect("guaranteed"))
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

        let expr = Expr::symbol(actual_id, actual_path);
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
                Expr::literal(lit)
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
                Expr::group(left)
            }
            TokenKind::Punctuator(LeftBrace) => {
                let stmts = self.parse_expr_block().unwrap();
                self.check(&TokenKind::Punctuator(RightBrace), -1)
                    .expect("Expected '}' after expression");
                stmts
            }
            unhandled => {
                panic!("unkown token: {:?}", &unhandled)
            }
        };
        Ok(expr)
    }
    fn parse_expr_fn_call(&mut self) -> PResult<Expr> {
        use PunctuatorKind::*;

        let callee_name = self.parse_identifier().unwrap();

        let callee = Expr::symbol(callee_name, None);

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

        let expr = Expr::fn_call(callee, arguments);
        Ok(expr)
    }
}
