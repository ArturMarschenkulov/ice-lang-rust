use crate::ast::{Field, Item, ItemKind, Parameter, Stmt, StmtKind, Ty, TyKind};
use crate::parser::Parser;
use crate::token::*;

/// This impl block is for parsing items.
impl Parser {
    pub fn parse_item_type_struct(&mut self) -> Box<Item> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        self.eat(&Keyword(Type)).unwrap();
        let name = self.eat_with(&TokenKind::is_identifier).unwrap().clone();

        let _eq_sign = self.eat(&Punctuator(Equal)).unwrap();

        self.eat(&Keyword(Struct)).unwrap();
        self.eat(&Punctuator(LeftBrace)).unwrap();

        let fields = self.parse_delim_seq(
            &Punctuator(LeftBrace),
            &Punctuator(RightBrace),
            &Punctuator(Comma),
            ("field", "name", "type"),
        );
        let fields = fields
            .iter()
            .map(|(name, ty)| Field {
                name: name.clone(),
                ty: TyKind::Simple(ty.clone()),
            })
            .collect::<Vec<_>>();

        let strct = ItemKind::Struct { name, fields };
        Box::from(Item { kind: strct })
    }
    pub fn parse_item_type(&mut self) -> Box<Item> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        // NOTE: Right now no tokens are consumed in this function
        //       This should be delegated for now to other functions.
        //       This function only finds one to which function to dispatch to

        self.check(&Keyword(Type), 0).unwrap();
        let _ = self
            .check_with(1, &TokenKind::is_identifier)
            .unwrap()
            .clone();
        let _eq_sign = self.check(&Punctuator(Equal), 2).unwrap();

        let ty = match self.peek(3).unwrap().kind {
            Keyword(Struct) => self.parse_item_type_struct(),
            _ => (todo!()),
        };
        ty
    }
    pub fn parse_item(&mut self) -> Box<Item> {
        use KeywordKind::*;
        use TokenKind::*;

        match &self.peek(0).unwrap().kind {
            Keyword(Fn) => self.parse_item_fn(),
            Keyword(Type) => self.parse_item_type(),
            _ => panic!("unexpected token"),
        }
    }

    /// There 3 variable declaration types.
    /// 1. var <ident> := <expr>;
    /// 2. var <ident> : <type> = <expr>;
    /// 3. var <ident> : <type>;
    pub fn parse_stmt_var(&mut self) -> Box<Stmt> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        self.eat(&Keyword(Var)).unwrap();

        let name = self
            .eat_identifier()
            .expect("Expected variable name")
            .clone();

        self.eat(&Punctuator(Colon)).unwrap();
        let ty = self.eat_identifier().cloned().ok().map(|ty| Ty {
            kind: TyKind::Simple(ty),
        });

        let expr = match self.eat(&Punctuator(Equal)) {
            Ok(_) => Some(self.parse_expr()),
            Err(_) if ty.is_some() => None,
            Err(_) => panic!("Expected equal sign"),
        };

        let var_decl = StmtKind::Var {
            var: name,
            ty,
            init: expr,
        };

        self.check(&Punctuator(Semicolon), 0)
            .expect("Expected ';' after variable declaration");

        Box::from(Stmt { kind: var_decl })
    }
    fn parse_delim_seq(
        &mut self,
        start: &TokenKind,
        end: &TokenKind,
        sep: &TokenKind,
        s: (&str, &str, &str),
    ) -> Vec<(Token, Token)> {
        use PunctuatorKind::*;
        use TokenKind::*;

        let mut result = Vec::new();
        self.eat(start).unwrap();
        while self.check(end, 0).is_err() {
            let name = self
                .eat_identifier()
                .unwrap_or_else(|_| panic!("Expected {} {}", s.0, s.1))
                .clone();
            let _ = self
                .eat(&Punctuator(Colon))
                .unwrap_or_else(|_| panic!("Expected ':' after {} {}", s.0, s.1));
            let ty = self
                .eat_identifier()
                .unwrap_or_else(|_| panic!("Expected {} {}", s.0, s.2))
                .clone();

            match self.eat(sep) {
                Ok(..) => (),
                Err(..) => {
                    let _ = self.check(end, 0).unwrap_or_else(|_| {
                        panic!("Expected '{}' after {} {}", end.as_str(), s.0, s.1)
                    });
                }
            }
            result.push((name, ty));
        }
        self.eat(end)
            .unwrap_or_else(|_| panic!("Expected '{}' after {} parameters", end.as_str(), s.0));
        result
    }
    pub fn parse_item_fn(&mut self) -> Box<Item> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        self.eat(&Keyword(Fn)).unwrap();

        let name = self
            .eat_identifier()
            .expect("Expected function name")
            .clone();

        let parameters = self
            .parse_delim_seq(
                &Punctuator(LeftParen),
                &Punctuator(RightParen),
                &Punctuator(Comma),
                ("parameter", "name", "type"),
            )
            .iter()
            .map(|(name, ty)| Parameter {
                name: name.clone(),
                ty: TyKind::Simple(ty.clone()),
            })
            .collect::<Vec<Parameter>>();

        let ret_type = self
            .eat(&Punctuator(Colon))
            .ok()
            .cloned()
            .map(|_t| {
                self.eat_identifier().cloned().ok().unwrap_or_else(|| {
                    let l_paren = self.eat(&Punctuator(LeftParen)).unwrap().clone();
                    let r_paren = self.eat(&Punctuator(RightParen)).unwrap().clone();
                    Token::from_self_slice(&[l_paren, r_paren])
                })
            })
            .map(|i| Ty {
                kind: TyKind::Simple(i),
            });

        let body = self
            .check(&Punctuator(LeftBrace), 0)
            .ok()
            .or_else(|| {
                panic!("{}", "Expected '{' after function name");
            })
            .cloned()
            .map(|_t| {
                let b = self.parse_expr_block();
                self.eat(&Punctuator(RightBrace))
                    .expect("Expected '}' after function body");

                b
            })
            .unwrap();

        // self.check(&Punctuator(LeftBrace), 0)
        //     .expect("Expected '{' after function name");
        // let block = self.parse_expr_block();
        // self.eat(&Punctuator(RightBrace))
        //     .expect("Expected '}' after function body");
        Box::from(Item {
            kind: ItemKind::Fn {
                name,
                params: parameters,
                ret: ret_type,
                body,
            },
        })
    }
}
