use super::super::lexer::token::{KeywordKind as KK, PunctuatorKind as PK, TokenKind as TK};
use super::ast::{Expr, Identifier, Literal, Operator, Stmt};

use super::{Error, PResult, Parser};

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

impl PartialEq for BindingPower {
    fn eq(&self, other: &Self) -> bool {
        self.right == other.left
    }
}

impl PartialOrd for BindingPower {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.right == other.left {
            Some(std::cmp::Ordering::Equal)
        } else if self.right > other.left {
            Some(std::cmp::Ordering::Greater)
        } else if self.right < other.left {
            Some(std::cmp::Ordering::Less)
        } else {
            None
        }
    }
    fn lt(&self, other: &Self) -> bool {
        self.right < other.left
    }
    fn gt(&self, other: &Self) -> bool {
        self.right > other.left
    }
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
        use Associativity as Ass;

        let arr = [
            //
            (PK::Asterisk, 7, Ass::Left),
            (PK::Slash, 7, Ass::Left),
            (PK::Percent, 7, Ass::Left),
            //
            (PK::Plus, 6, Ass::Left),
            (PK::Minus, 6, Ass::Left),
            //
            (PK::Less, 4, Ass::None),
            (PK::LessEqual, 4, Ass::None),
            (PK::Greater, 3, Ass::None),
            (PK::GreaterEqual, 4, Ass::None),
            (PK::EqualEqual, 4, Ass::None),
            (PK::BangEqual, 4, Ass::None),
            //
            (PK::Ampersand, 3, Ass::None),
            (PK::VerticalBar, 2, Ass::None),
            //
            (PK::Equal, 1, Ass::Right),
        ];
        // Workaround, so that we don't have to implement stuff only for the HashMap.
        arr.iter()
            .map(|(pk, prec, asso)| (format!("{:?}", TK::Punctuator(pk.clone())), BindingPower::from(*prec, *asso)))
            .collect::<HashMap<String, BindingPower>>()
    }
    pub fn postfix_binding_powers() -> std::collections::HashMap<String, BindingPower> {
        use std::collections::HashMap;
        let arr: [(PK, u32, Associativity); 0] = [];
        arr.iter()
            .map(|(pk, prec, asso)| (format!("{:?}", TK::Punctuator(pk.clone())), BindingPower::from(*prec, *asso)))
            .collect::<HashMap<String, BindingPower>>()
    }
    pub fn prefix_binding_powers() -> std::collections::HashMap<String, BindingPower> {
        use std::collections::HashMap;
        use Associativity as Ass;

        let arr = [
            (PK::Plus, 6, Ass::Right),
            (PK::Minus, 6, Ass::Right),
        ];
        arr.iter()
            .map(|(pk, prec, asso)| (format!("{:?}", TK::Punctuator(pk.clone())), BindingPower::from(*prec, *asso)))
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
    this.check(&TK::Punctuator(PK::LeftBrace), 0).expect(text_0);
    let block = this.parse_expr_block().unwrap();
    this.check(&TK::Punctuator(PK::RightBrace), -1)
        .expect(text_1);
    Ok(block)
}

/// This impl block is for parsing control flow expressions
impl Parser {
    fn parse_expr_if(&mut self) -> PResult<Expr> {
        self.eat(&TK::Keyword(KK::If)).expect("Expected 'if'");
        let condition = self.parse_expr()?;
        let then_branch = parse_expr_block_(
            self,
            "Expected '{' after if condition",
            "Expected '}' after if body",
        )
        .unwrap();

        let else_branch = match self.peek(0).unwrap().kind {
            TK::Keyword(KK::Else) => Some(self.parse_expr_else().unwrap()),
            _ => None,
        };
        Ok(Expr::if_(condition, then_branch, else_branch))
    }
    fn parse_expr_else(&mut self) -> PResult<Expr> {
        self.eat(&TK::Keyword(KK::Else)).expect("Expected 'else'");
        match self.peek(0).unwrap().kind {
            TK::Keyword(KK::If) => self.parse_expr_if(),
            _ => parse_expr_block_(
                self,
                "Expected '{' after else",
                "Expected '}' after else body",
            ),
        }
    }
    fn parse_expr_while(&mut self) -> PResult<Expr> {
        self.eat(&TK::Keyword(KK::While)).unwrap();
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
        self.eat(&TK::Keyword(KK::For)).unwrap();
        let initilizer = match self.peek(0).unwrap().kind {
            TK::Keyword(KK::Var) => Some(self.parse_stmt_var().unwrap()),
            TK::Punctuator(PK::Semicolon) => {
                self.advance();
                None
            }
            _ => Some(self.parse_stmt_expression().unwrap()),
        };

        let condition = self.parse_expr().unwrap();
        self.eat(&TK::Punctuator(PK::Semicolon))
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
        self.eat(&TK::Punctuator(PK::LeftBrace)).unwrap();
        let mut statements: Vec<Stmt> = Vec::new();

        while !self.is_at_end() && self.check(&TK::Punctuator(PK::RightBrace), 0).is_err() {
            statements.push(self.parse_stmt().unwrap());
        }

        self.eat(&TK::Punctuator(PK::RightBrace)).unwrap();
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

        let mut left = if un_prefix.kind.can_be_operator() {
            let un_prefix = Operator::try_from(un_prefix).map_err(|x| {
                super::Error::new(format!("Expected unary prefix operator, got {:?}", x))
            })?;

            match prefix_binding_power(self, &un_prefix) {
                Some(bp) if prev_bp.right < bp.left => {
                    let op = self.parse_operator()?;
                    let right = self.parse_expr_binary(bp)?;

                    Expr::unary_prefix(op, right)
                }
                Some(_) | None => self.parse_expr_primary()?,
            }
        } else {
            self.parse_expr_primary()?
        };

        loop {
            let bin_infix = self.peek(0).unwrap();
            // let bin_infix = Operator::try_from(bin_infix.clone()).unwrap();
            let bin_infix = if let Ok(bin_infix) = Operator::try_from(bin_infix.clone()) {
                bin_infix
            } else {
                break;
            };
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
                    let operator = self.parse_operator()?;
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

    fn parse_operator(&mut self) -> PResult<Operator> {
        self.eat_punctuator()
            .map_err(|token| Error::new(format!("Expected operator, got {:?}", token)))
            .map(|token| Operator::try_from(token.clone()).expect("guaranteed"))
    }
    fn parse_symbol(&mut self) -> PResult<Expr> {
        let mut ids = Vec::new();
        ids.push(self.parse_identifier()?);

        while self.eat(&TK::Punctuator(PK::ColonColon)).is_ok() {
            ids.push(self.parse_identifier()?);
        }

        let actual_id = ids.pop().expect("Expected identifier");
        let actual_path = if ids.is_empty() { None } else { Some(ids) };

        Ok(Expr::symbol(actual_id, actual_path))
    }
    fn parse_expr_primary(&mut self) -> PResult<Expr> {
        let token = self.peek(0).unwrap().clone();

        let expr = match token.kind {
            TK::Literal(lit) => {
                self.advance();
                let lit = Literal::from(lit);
                Expr::literal(lit)
            }
            TK::Identifier(_) => {
                if self.check(&TK::Punctuator(PK::LeftParen), 1).is_ok() {
                    self.parse_expr_fn_call().unwrap()
                } else {
                    self.parse_symbol()?
                }
            }

            TK::Keyword(KK::If) => self.parse_expr_if().unwrap(),
            TK::Keyword(KK::While) => self.parse_expr_while().unwrap(),
            TK::Keyword(KK::For) => self.parse_expr_for().unwrap(),
            TK::Punctuator(PK::LeftParen) => {
                self.eat(&TK::Punctuator(PK::LeftParen))
                    .expect("Expected '(' before expression");
                let left = self.parse_expr().unwrap();
                self.eat(&TK::Punctuator(PK::RightParen))
                    .expect("Expected ')' after expression");
                Expr::group(left)
            }
            TK::Punctuator(PK::LeftBrace) => {
                let stmts = self.parse_expr_block().unwrap();
                self.check(&TK::Punctuator(PK::RightBrace), -1)
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
        let callee_name = self.parse_identifier()?;

        let callee = Expr::symbol(callee_name, None);

        self.eat(&TK::Punctuator(PK::LeftParen))
            .expect("Expected '(' after function name");

        let mut arguments: Vec<Expr> = Vec::new();
        while self.check(&TK::Punctuator(PK::RightParen), 0).is_err() {
            arguments.push(self.parse_expr().unwrap());
            if self.check(&TK::Punctuator(PK::RightParen), 0).is_ok() {
                self.eat(&TK::Punctuator(PK::Comma))
                    .expect("Expected ',' after argument");
            }
        }

        self.eat(&TK::Punctuator(PK::RightParen))
            .expect("Expected ')' after arguments");

        let expr = Expr::fn_call(callee, arguments);
        Ok(expr)
    }
}

mod tests {
    use super::super::super::lexer;
    use super::*;
    #[test]
    fn test_expr_binary() {
        let txt = "1 + 2";
        let tokens = lexer::lex_tokens_from_file(txt).unwrap();
        let mut parser = Parser::new(tokens.tokens);

        let bin = parser.parse_expr_binary(BindingPower::new()).unwrap();
        // let tree = Expr::binary_infix(
        //     Expr::literal(LK::integer("1", None, None)),
        //     Operator { name: Token::new(TK::Punctuator(PK::Plus), 0, 1) } },
        //     Expr::literal(LK::integer("2", None, None)),
        // );

        // assert_eq!(
        //     bin,
        //     Expr {
        //         kind: ExprKind::Literal(LiteralKind::Number(1.0))
        //     }
        // );
    }
}
