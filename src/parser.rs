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
            statements.push(*self.parse_stmt_declaration());
        }
        statements
    }
    fn parse_stmt_declaration(&mut self) -> Box<Stmt> {
        let token = self.peek(0);
        match &token.kind {
            TokenKind::Keyword(KeywordKind::Var) => {
                self.advance();
                self.parse_stmt_declaration_var()
            }
            TokenKind::Keyword(KeywordKind::Fn) => {
                self.advance();
                self.parse_stmt_declaration_fn()
            }
            TokenKind::Punctuator(PunctuatorKind::Semicolon) => {
                self.advance();
                Box::from(Stmt::NoOperation)
            }
            _ => self.parse_stmt(),
        }
    }
    fn parse_stmt_declaration_var(&mut self) -> Box<Stmt> {
        let name = self.consume_identifier("Expected variable name.").unwrap();
        let token = self.peek(0);
        let init = match token.kind {
            TokenKind::Punctuator(PunctuatorKind::Equal) => {
                self.advance();
                Some(self.parse_expr())
            }
            _ => None,
        };
        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::Semicolon),
            "Expected ';' after variable declaration.",
        );
        Box::from(Stmt::VarDeclaration(name, init))
    }
    fn parse_stmt_declaration_fn(&mut self) -> Box<Stmt> {
        let name = self.consume_identifier("Expected function name.").unwrap();
        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::LeftParen),
            "Expected '(' after function name')",
        );

        let mut _token = self.peek(0).clone();
        let mut parameters: Vec<Token> = Vec::new();

        if self.peek(0).kind != TokenKind::Punctuator(PunctuatorKind::RightParen) {
            while {
                let _para = self.consume_identifier("Expected parameter");
                if let Ok(para) = _para {
                    parameters.push(para);
                }
                let res = self.peek(0).kind == TokenKind::Punctuator(PunctuatorKind::Comma);
                if res {
                    self.advance();
                }
                res
            } {}
        }
        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::RightParen),
            "Expected ')' after parameters",
        );

        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::LeftBrace),
            "Expected '{' after function part",
        );
        let block = self.parse_expr_block();
        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::RightBrace),
            "Expect '}' after function body.",
        );
        Box::from(Stmt::FnDeclaration(name, parameters, block))
    }
    fn parse_stmt(&mut self) -> Box<Stmt> {
        let token = self.peek(0);
        match &token.kind {
            TokenKind::Keyword(KeywordKind::Print) => {
                self.advance();
                self.parse_stmt_print()
            }
            TokenKind::Keyword(KeywordKind::Println) => {
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
        self.parse_expr_assignment()
    }
    fn parse_expr_if(&mut self) -> Box<Expr> {
        self.advance(); //skip the if
        let condition = self.parse_expr();
        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::LeftBrace),
            "Expect '{' after if condition.",
        );
        let then_branch = self.parse_expr_block();
        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::RightBrace),
            "Expect '}' after then branch.",
        );
        let else_branch = match self.peek(0).kind {
            TokenKind::Keyword(KeywordKind::Else) => {
                self.advance();
                Some(self.parse_expr_else())
            }
            _ => None,
        };
        Box::from(Expr::If(condition, then_branch, else_branch))
    }
    fn parse_expr_else(&mut self) -> Box<Expr> {
        match self.peek(0).kind {
            TokenKind::Keyword(KeywordKind::If) => {
                //self.advance();
                self.parse_expr_if()
            }
            _ => {
                self.consume(
                    &TokenKind::Punctuator(PunctuatorKind::LeftBrace),
                    "Expect '{' after else condition.",
                );
                let e = self.parse_expr_block();
                self.consume(
                    &TokenKind::Punctuator(PunctuatorKind::RightBrace),
                    "Expect '}' after else branch.",
                );
                e
            }
        }
    }
    fn parse_expr_while(&mut self) -> Box<Expr> {
        self.advance(); //skip the while
        let condition = self.parse_expr();
        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::LeftBrace),
            "Expect '{' after while condition.",
        );
        let while_body = self.parse_expr_block();
        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::RightBrace),
            "Expect '}' after while body.",
        );
        Box::from(Expr::While(condition, while_body))
    }
    fn parse_expr_for(&mut self) -> Box<Expr> {
        self.advance(); //skip the for
        let initilizer = match self.peek(0).kind {
            TokenKind::Keyword(KeywordKind::Var) => {
                self.advance();
                Some(self.parse_stmt_declaration_var())
            }
            TokenKind::Punctuator(PunctuatorKind::Semicolon) => {
                self.advance();
                None
            }
            _ => Some(self.parse_stmt_expression()),
        };

        let condition = self.parse_expr();
        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::Semicolon),
            "Expect ';' after for condition.",
        );

        let itr_expr = self.parse_expr();
        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::LeftBrace),
            "Expect '{' after for interation expression.",
        );

        let while_body = self.parse_expr_block();
        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::RightBrace),
            "Expect '}' after while body.",
        );
        Box::from(Expr::For(
            initilizer.unwrap(),
            condition,
            itr_expr,
            while_body,
        ))
    }
    fn parse_expr_block(&mut self) -> Box<Expr> {
        // Helper function (or rather closure).
        let should_add = |sel: &mut Self| -> bool {
            use PunctuatorKind::*;
            use TokenKind::*;
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
        while !self.is_at_end()
            && (self.peek(0).kind != TokenKind::Punctuator(PunctuatorKind::RightBrace))
        {
            if should_add(self) {
                statements.push(*self.parse_stmt_declaration());
            } else {
                break;
            }
        }

        let expr = if self.tokens[self.current].kind
            == TokenKind::Punctuator(PunctuatorKind::RightBrace)
        {
            None
        } else {
            Some(self.parse_expr())
        };

        Box::from(Expr::Block(statements, expr))
    }
    fn parse_expr_assignment(&mut self) -> Box<Expr> {
        if let TokenKind::Identifier(_) = self.peek(0).kind {
            match self.peek(1).kind {
                TokenKind::Punctuator(PunctuatorKind::Equal)
                | TokenKind::Punctuator(PunctuatorKind::PlusEqual)
                | TokenKind::Punctuator(PunctuatorKind::MinusEqual)
                | TokenKind::Punctuator(PunctuatorKind::StarEqual)
                | TokenKind::Punctuator(PunctuatorKind::SlashEqual) => {
                    let identifier_token = self.peek(0).clone();
                    self.advance();
                    let operator_token = self.peek(0).clone();
                    self.advance();
                    let right = self.parse_expr_assignment();
                    return Box::from(Expr::Assign(identifier_token, operator_token, right));
                }
                _ => {}
            }
        };
        self.parse_expr_binary(0)
    }
    fn parse_expr_binary(&mut self, parent_precedence: i32) -> Box<Expr> {
        let unary_operator_precedence = get_unary_operator_precedence(self.peek(0));
        let mut left =
            if unary_operator_precedence != 0 && unary_operator_precedence >= parent_precedence {
                let operator = self.peek(0).clone();
                self.advance();
                let right = self.parse_expr_binary(unary_operator_precedence);
                Box::from(Expr::Unary(operator, right))
            } else {
                self.parse_expr_primary()
            };

        loop {
            let precedence = get_binary_operator_precedence(self.peek(0));
            if precedence == 0 || precedence <= parent_precedence {
                break;
            }
            let operator = self.peek(0).clone();
            self.advance();
            let right = self.parse_expr_binary(precedence);
            left = Box::from(Expr::Binary(left, operator.clone(), right));
        }
        left
    }
    fn parse_expr_primary(&mut self) -> Box<Expr> {
        let token = self.peek(0).clone();

        let expr = match token.kind {
            TokenKind::Keyword(KeywordKind::False) => {
                self.advance();
                Expr::Literal(LiteralKind::Boolean(false))
            }
            TokenKind::Keyword(KeywordKind::True) => {
                self.advance();
                Expr::Literal(LiteralKind::Boolean(true))
            }
            TokenKind::Literal(LiteralKind::Integer(num)) => {
                self.advance();
                Expr::Literal(LiteralKind::Integer(num))
            }
            TokenKind::Identifier(_) => {
                if self.peek(1).clone().kind == TokenKind::Punctuator(PunctuatorKind::LeftParen) {
                    //self.advance();
                    *self.parse_expr_fn_call()
                    //*self._call()
                } else {
                    self.advance();
                    Expr::Symbol(self.peek(-1).clone())
                }
            }
            TokenKind::Literal(LiteralKind::String(s)) => {
                self.advance();
                Expr::Literal(LiteralKind::String(s))
            }
            TokenKind::Keyword(KeywordKind::If) => *self.parse_expr_if(),
            TokenKind::Keyword(KeywordKind::While) => *self.parse_expr_while(),
            TokenKind::Keyword(KeywordKind::For) => *self.parse_expr_for(),
            TokenKind::Punctuator(PunctuatorKind::LeftParen) => {
                self.advance(); //skip the (
                let left = self.parse_expr();
                self.consume(
                    &TokenKind::Punctuator(PunctuatorKind::RightParen),
                    "Expect ')' after expression4",
                );
                Expr::Grouping(left)
            }
            TokenKind::Punctuator(PunctuatorKind::LeftBrace) => {
                self.advance(); //skip the {
                let stmts = self.parse_expr_block();
                self.consume(
                    &TokenKind::Punctuator(PunctuatorKind::RightBrace),
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
        let callee = Box::from(Expr::Symbol(self.peek(0).clone()));
        self.advance();
        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::LeftParen),
            "Expect '(' after function name.",
        );

        let mut arguments: Vec<Expr> = Vec::new();
        while self.peek(0).kind != TokenKind::Punctuator(PunctuatorKind::RightParen) {
            arguments.push(*self.parse_expr());
            if self.peek(0).kind != TokenKind::Punctuator(PunctuatorKind::RightParen) {
                self.consume(
                    &TokenKind::Punctuator(PunctuatorKind::Comma),
                    "Expect ',' after argument.",
                );
            }
        }

        self.consume(
            &TokenKind::Punctuator(PunctuatorKind::RightParen),
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
    fn consume_identifier(&mut self, message: &str) -> Result<Token, String> {
        // TODO: This looks messy. Fix it!
        let token = self.peek(0).clone();
        let token_kind = token.kind.clone();
        if let TokenKind::Identifier(_) = token_kind {
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
        self.peek(0).kind == TokenKind::SpecialKeyword(SpecialKeywordKind::Eof)
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
