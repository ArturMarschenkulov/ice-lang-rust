use crate::ast::{Expr, Stmt};
use crate::token::*;

#[allow(unused_macros)]
macro_rules! get_function_name {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        &name[..name.len() - 3]
    }};
}
//println!("{}", get_function_name!());

/*
stmt_declaration
    := stmt
    | stmt_declaration_var
    | stmt_declaration_struct
    | stmt_declaration_fn
    | ';'
stmt_declaration_var
    := 'var' (':' type) ('=' expr) ';'

stmt_declaration_fn
    := 'fn' identifier '(' parameters? ')' expr_block ';'
stmt
    := stmt_print
    | stmt_expression
stmt_print
    := expr ';'
stmt_expression
    := expr ';'

expr
    := expr_if
    | expr_while
    | expr_block
    | expr_assignment

expr_assignment
    := identifier '=' expr

expr_if
    := 'if' expr expr_block expr_else?
expr_else
    := 'else' (expr_if | expr_block)
expr_while
    := 'while' expr expr_block
expr_for
    := 'for' (stmt_declaration_var) ';' (expr) ';' (expr) expr_block


expr_block := '{' (stmt_declaration)* expr '}'
expr_unary := expr operator
expr_binary := expr operator expr
expr_primary := ident | 'true' | 'false' | expr_group | string | number

 */

/*
   prodtype, sumtype, subtype

*/

pub fn get_ast_from_tokens(tokens: Vec<Token>) -> Vec<Stmt> {
    Parser::new().parse(tokens)
}

fn get_unary_operator_precedence(token: &Token) -> i32 {
    use PunctuatorKind::*;
    use TokenKind::*;
    match token.kind {
        Punctuator(Plus) | Punctuator(Minus) | Punctuator(Bang) => 6,
        _ => 0,
    }
}
fn get_binary_operator_precedence(token: &Token) -> i32 {
    use PunctuatorKind::*;
    use TokenKind::*;
    match token.kind {
        Punctuator(Star) | Punctuator(Slash) | Punctuator(Percent) => 5,
        Punctuator(Plus) | Punctuator(Minus) => 4,
        Punctuator(Less)
        | Punctuator(LessEqual)
        | Punctuator(Greater)
        | Punctuator(GreaterEqual)
        | Punctuator(EqualEqual)
        | Punctuator(BangEqual) => 3,
        Punctuator(Ampersand) | Punctuator(AmpersandAmpersand) => 2,
        Punctuator(Pipe) | Punctuator(PipePipe) => 1,
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
        (Punctuator(Star), 7, Left),
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
        (Punctuator(Pipe), 2, None),
        //
        (Punctuator(Equal), 1, Right),
    ];

    let s = |tk: &TokenKind| format!("{:?}", tk);
    arr.iter()
        .map(|(tk, prec, asso)| (s(tk), BindingPower::new(*prec, *asso)))
        .collect::<HashMap<String, BindingPower>>()
        .get(&s(&token.kind))
        .unwrap_or(&BindingPower::new(0, None))
        .clone()
}
struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new() -> Self {
        Self {
            tokens: Vec::new(),
            current: 0,
        }
    }
    fn parse(&mut self, tokens: Vec<Token>) -> Vec<Stmt> {
        self.tokens = tokens;
        let mut statements: Vec<Stmt> = Vec::new();

        while !self.is_at_end() {
            statements.push(*self.parse_stmt_decl());
        }
        statements
    }
    fn parse_stmt_decl(&mut self) -> Box<Stmt> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        let token = self.peek(0);
        match &token.kind {
            Keyword(Var) => self.parse_stmt_decl_var(),
            Keyword(Fn) => self.parse_stmt_decl_fn(),
            Punctuator(Semicolon) => {
                self.advance();
                Box::from(Stmt::NoOperation)
            }
            _ => self.parse_stmt(),
        }
    }
    fn parse_stmt_decl_var(&mut self) -> Box<Stmt> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        assert!(self.peek(0).kind == Keyword(Var));
        self.advance();

        //if let Literal(lit_name) = self.peek(0).kind {}
        let name = self.consume_identifier("Expected variable name.").unwrap();

        let token = self.peek(0);
        let mut ty: Option<String> = None;
        let init = match &token.kind {
            Punctuator(Colon) => {
                self.advance();

                let token = self.peek(0);
                if let Identifier(t) = &token.kind {
                    ty = Some(t.clone());
                    self.advance();
                }

                let token = self.peek(0);
                match &token.kind {
                    Punctuator(Equal) => {
                        self.advance();
                        Some(self.parse_expr())
                    }
                    _ => None,
                }
            }
            _ => None,
        };
        self.consume(
            &Punctuator(Semicolon),
            "Expected ';' after variable declaration.",
        );
        Box::from(Stmt::VarDeclaration(name, ty, init))
    }
    fn parse_stmt_decl_fn(&mut self) -> Box<Stmt> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        assert!(self.peek(0).kind == Keyword(Fn));
        self.advance();
        //let s = self.consume_ident().
        let name = self.consume_identifier("Expected function name.").unwrap();
        self.consume(&Punctuator(LeftParen), "Expected '(' after function name')");

        let mut _token = self.peek(0).clone();
        let mut parameters: Vec<(Token, String)> = Vec::new();
        if self.peek(0).kind != Punctuator(RightParen) {
            while {
                let _para = self.consume_identifier("Expected parameter").clone();
                self.consume(&Punctuator(Colon), "Expected ':' after parameter name");
                let _ty = self.consume_identifier("Expected type after ':'");
                //let l = (_para, _ty);
                if let Ok(para) = _para {
                    parameters.push((para, "".to_string()));
                }
                let res = self.peek(0).kind == Punctuator(Comma);
                if res {
                    self.advance();
                }
                res
            } {}
        }
        self.consume(&Punctuator(RightParen), "Expected ')' after parameters");

        self.consume(&Punctuator(LeftBrace), "Expected '{' after function part");
        let block = self.parse_expr_block();
        self.consume(&Punctuator(RightBrace), "Expect '}' after function body.");
        Box::from(Stmt::FnDeclaration(name, parameters, block))
    }
    fn parse_stmt(&mut self) -> Box<Stmt> {
        use KeywordKind::*;
        use TokenKind::*;
        let token = self.peek(0);
        match &token.kind {
            Keyword(Print) => {
                self.advance();
                self.parse_stmt_print()
            }
            Keyword(Println) => {
                self.advance();
                self.parse_stmt_println()
            }
            _ => self.parse_stmt_expression(),
        }
    }
    fn parse_stmt_expression(&mut self) -> Box<Stmt> {
        let expr = self.parse_expr();
        //("{:?}", &self.tokens[self.current]);
        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::Semicolon),
            "Expect ';' after expression1",
        );

        Box::from(Stmt::Expression(expr))
    }
    fn parse_stmt_print(&mut self) -> Box<Stmt> {
        let expr = self.parse_expr();
        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::Semicolon),
            "Expect ';' after expression2",
        );
        Box::from(Stmt::Print(expr))
    }
    fn parse_stmt_println(&mut self) -> Box<Stmt> {
        let expr = self.parse_expr();
        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::Semicolon),
            "Expect ';' after expression3",
        );
        Box::from(Stmt::Println(expr))
    }
    fn parse_expr(&mut self) -> Box<Expr> {
        self.parse_expr_binary(0.0)
    }
    fn parse_expr_if(&mut self) -> Box<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        self.advance(); //skip the if
        let condition = self.parse_expr();
        self.consume(&Punctuator(LeftBrace), "Expect '{' after if condition.");
        let then_branch = self.parse_expr_block();
        self.consume(&Punctuator(RightBrace), "Expect '}' after then branch.");
        let else_branch = match self.peek(0).kind {
            Keyword(Else) => {
                self.advance();
                Some(self.parse_expr_else())
            }
            _ => None,
        };
        Box::from(Expr::If(condition, then_branch, else_branch))
    }
    fn parse_expr_else(&mut self) -> Box<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;
        match self.peek(0).kind {
            Keyword(If) => {
                //self.advance();
                self.parse_expr_if()
            }
            _ => {
                self.consume(&Punctuator(LeftBrace), "Expect '{' after else condition.");
                let e = self.parse_expr_block();
                self.consume(&Punctuator(RightBrace), "Expect '}' after else branch.");
                e
            }
        }
    }
    fn parse_expr_while(&mut self) -> Box<Expr> {
        use PunctuatorKind::*;
        use TokenKind::*;
        self.advance(); //skip the while
        let condition = self.parse_expr();
        self.consume(&Punctuator(LeftBrace), "Expect '{' after while condition.");
        let while_body = self.parse_expr_block();
        self.consume(&Punctuator(RightBrace), "Expect '}' after while body.");
        Box::from(Expr::While(condition, while_body))
    }
    fn parse_expr_for(&mut self) -> Box<Expr> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;
        self.advance(); //skip the for
        let initilizer = match self.peek(0).kind {
            Keyword(Var) => Some(self.parse_stmt_decl_var()),
            Punctuator(Semicolon) => {
                self.advance();
                None
            }
            _ => Some(self.parse_stmt_expression()),
        };

        let condition = self.parse_expr();
        self.consume(&Punctuator(Semicolon), "Expect ';' after for condition.");

        let itr_expr = self.parse_expr();
        self.consume(
            &Punctuator(LeftBrace),
            "Expect '{' after for interation expression.",
        );

        let while_body = self.parse_expr_block();
        self.consume(&Punctuator(RightBrace), "Expect '}' after while body.");
        Box::from(Expr::For(
            initilizer.unwrap(),
            condition,
            itr_expr,
            while_body,
        ))
    }
    fn parse_expr_block(&mut self) -> Box<Expr> {
        use PunctuatorKind::*;
        use TokenKind::*;
        // Helper function (or rather closure).
        let should_add = |sel: &mut Self| -> bool {
            let mut pointer = sel.current;
            while sel.tokens[pointer].kind != Punctuator(RightBrace) {
                if sel.tokens[pointer].kind == Punctuator(Semicolon) {
                    return true;
                }
                pointer += 1;
            }
            false
        };
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.is_at_end() && (self.peek(0).kind != Punctuator(RightBrace)) {
            if should_add(self) {
                statements.push(*self.parse_stmt_decl());
            } else {
                break;
            }
        }

        let expr = if self.tokens[self.current].kind == Punctuator(RightBrace) {
            None
        } else {
            Some(self.parse_expr())
        };

        Box::from(Expr::Block(statements, expr))
    }
    fn parse_expr_binary(&mut self, prev_bp: f32) -> Box<Expr> {
        let un_op_prec = get_unary_operator_precedence(self.peek(0));
        let mut left = if un_op_prec != 0 && un_op_prec >= prev_bp as i32 {
            let op = self.peek(0).clone();
            self.advance();
            let right = self.parse_expr_binary(un_op_prec as f32);
            Box::from(Expr::Unary(op, right))
        } else {
            self.parse_expr_primary()
        };

        loop {
            let binding_power = get_infix_binding_power(self.peek(0));
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

            let operator = self.peek(0).clone();
            self.advance();
            let right = self.parse_expr_binary(binding_power.right);
            left = Box::from(Expr::Binary(left, operator.clone(), right));
        }
        left
    }
    fn parse_expr_primary(&mut self) -> Box<Expr> {
        use KeywordKind::*;
        use LiteralKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;
        let token = self.peek(0).clone();

        let expr = match token.kind {
            Keyword(False) => {
                self.advance();
                Expr::Literal(Boolean(false))
            }
            Keyword(True) => {
                self.advance();
                Expr::Literal(Boolean(true))
            }
            Literal(Integer { content, base }) => {
                self.advance();
                Expr::Literal(Integer { content, base })
            }
            Literal(Floating { content }) => {
                self.advance();
                Expr::Literal(Floating { content })
            }
            Identifier(_) => {
                if self.peek(1).clone().kind == Punctuator(PunctuatorKind::LeftParen) {
                    //self.advance();
                    *self.parse_expr_fn_call()
                    //*self._call()
                } else {
                    self.advance();
                    Expr::Symbol(self.peek(-1).clone())
                }
            }
            Literal(String(s)) => {
                self.advance();
                Expr::Literal(String(s))
            }
            Literal(Char(c)) => {
                self.advance();
                Expr::Literal(Char(c))
            }
            Keyword(If) => *self.parse_expr_if(),
            Keyword(While) => *self.parse_expr_while(),
            Keyword(For) => *self.parse_expr_for(),
            Punctuator(LeftParen) => {
                self.advance(); //skip the (
                let left = self.parse_expr();
                self.consume(&Punctuator(RightParen), "Expect ')' after expression4");
                Expr::Grouping(left)
            }
            Punctuator(LeftBrace) => {
                self.advance(); //skip the {
                let stmts = self.parse_expr_block();
                self.consume(
                    &Punctuator(PunctuatorKind::RightBrace),
                    "Expect '}' after block",
                );
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

        let callee = Box::from(Expr::Symbol(self.peek(0).clone()));
        self.advance();
        self.consume(
            &Punctuator(PunctuatorKind::LeftParen),
            "Expect '(' after function name.",
        );

        let mut arguments: Vec<Expr> = Vec::new();
        while self.peek(0).kind != Punctuator(RightParen) {
            arguments.push(*self.parse_expr());
            if self.peek(0).kind != Punctuator(RightParen) {
                self.consume(&Punctuator(Comma), "Expect ',' after argument.");
            }
        }

        self.consume(
            &Punctuator(RightParen),
            "Expect ')' after function arguments.",
        );

        Box::from(Expr::FnCall(callee, arguments))
    }
}
impl Parser {
    fn consume(&mut self, kind: &TokenKind, message: &str) {
        if &self.peek(0).kind == kind && !self.is_at_end() {
            self.advance();
        } else {
            //let f = format!("{:?}; {:?}; kind = {:?}, self.kind = {:?}", &self.peek(0), message, kind, self.peek(0).kind);
            let msg = format!("{} but instead found {:?}", message, &self.peek(0).kind);
            println!("{}", msg);
        }
    }
    fn consume_ident(&mut self) -> Result<Token, String> {
        todo!()
    }
    fn consume_identifier(&mut self, message: &str) -> Result<Token, String> {
        use TokenKind::*;
        // TODO: This looks messy. Fix it!
        let token = self.peek(0).clone();
        let token_kind = &token.kind;
        if let Identifier(_) = token_kind {
            self.advance();
            Ok(token)
        } else {
            let f = format!("{:?}; {:?}", &self.peek(0), message);
            println!("{}", &f);
            Err(f)
        }

        // if let TokenKind::Identifier(name) = self.peek(0).kind.clone() {
        //     self.advance();
        //     Ok(Token {
        //         kind: TokenKind::Identifier(name),
        //     })
        // } else {
        //     let f = format!("{:?}; {:?}", &self.peek(0), message);
        //     println!("{}", &f);
        //     Err(f)
        // }
    }
    fn advance(&mut self) {
        if !self.is_at_end() {
            self.current += 1;
        }
    }
    fn is_at_end(&self) -> bool {
        use SpecialKeywordKind::*;
        use TokenKind::*;
        self.peek(0).kind == SpecialKeyword(Eof)
    }
    fn peek(&self, offset: i32) -> &Token {
        let index = self.current as i32 + offset;
        if index as usize >= self.tokens.len() {
            panic!("qqqq");
            //&self.tokens[self.current]
        } else {
            &self.tokens[index as usize]
        }
    }
}
