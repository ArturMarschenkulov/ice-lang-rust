use super::Parser;
use crate::ast::{Field, Identifier, Item, ItemKind, Parameter, Stmt, StmtKind, Ty, TyKind};
use crate::token::*;

/// This impl block is for parsing items.
impl Parser {
    pub fn parse_item_type_struct(&mut self) -> Box<Item> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        //use TokenKind::*;

        self.eat(&TokenKind::Keyword(Type)).unwrap();
        let name = self.eat_with(&TokenKind::is_identifier).unwrap().clone();

        let _eq_sign = self.eat(&TokenKind::Punctuator(Equal)).unwrap();

        self.eat(&TokenKind::Keyword(Struct)).unwrap();
        self.eat(&TokenKind::Punctuator(LeftBrace)).unwrap();

        let fields = self.parse_delim_seq(
            &TokenKind::Punctuator(LeftBrace),
            &TokenKind::Punctuator(RightBrace),
            &TokenKind::Punctuator(Comma),
            ("field", "name", "type"),
        );
        let fields = fields
            .iter()
            .map(|(name, ty)| Field {
                name: Identifier::from_token(name.clone()),
                ty: TyKind::Simple(Identifier::from_token(ty.clone())),
            })
            .collect::<Vec<_>>();

        let strct = ItemKind::Struct {
            name: Identifier::from_token(name),
            fields,
        };
        Box::from(Item { kind: strct })
    }
    pub fn parse_item_type(&mut self) -> Box<Item> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        use TokenKind::*;

        // NOTE: Right now no tokens are consumed in this function
        //       This should be delegated for now to other functions.
        //       This function only finds one to which function to dispatch to

        let _ = self.check(&Keyword(Type), 0).unwrap();
        let _ = self.check_with(1, &TokenKind::is_identifier).unwrap();
        let _ = self.check(&Punctuator(Equal), 2).unwrap();

        match self.peek(3).unwrap().kind {
            Keyword(Struct) => self.parse_item_type_struct(),
            _ => (todo!()),
        }
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
        // use TokenKind::*;

        self.eat(&TokenKind::Keyword(Var)).unwrap();

        let name = self
            .eat_identifier()
            .expect("Expected variable name")
            .clone();

        self.eat(&TokenKind::Punctuator(Colon)).unwrap();
        let ty = self.eat_identifier().cloned().ok().map(|ty| Ty {
            kind: TyKind::Simple(Identifier::from_token(ty)),
        });

        let expr = match self.eat(&TokenKind::Punctuator(Equal)) {
            Ok(_) => Some(self.parse_expr()),
            Err(_) if ty.is_some() => None,
            Err(_) => panic!("Expected equal sign"),
        };

        let var_decl = StmtKind::Var {
            var: Identifier::from_token(name),
            ty,
            init: expr,
        };

        self.check(&TokenKind::Punctuator(Semicolon), 0)
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
        //use TokenKind::*;

        self.eat(&TokenKind::Keyword(Fn)).unwrap();

        let name = self
            .eat_identifier()
            .expect("Expected function name")
            .clone();

        let parameters = self
            .parse_delim_seq(
                &TokenKind::Punctuator(LeftParen),
                &TokenKind::Punctuator(RightParen),
                &TokenKind::Punctuator(Comma),
                ("parameter", "name", "type"),
            )
            .iter()
            .map(|(name, ty)| Parameter {
                name: Identifier::from_token(name.clone()),
                ty: TyKind::Simple(Identifier::from_token(ty.clone())),
            })
            .collect::<Vec<Parameter>>();

        let ret_type = self
            .eat(&TokenKind::Punctuator(Colon))
            .ok()
            .cloned()
            .map(|_x| self.parse_ty());

        let body = self
            .check(&TokenKind::Punctuator(LeftBrace), 0)
            .ok()
            .or_else(|| {
                panic!("{}", "Expected '{' after function name");
            })
            .cloned()
            .map(|_t| {
                let b = self.parse_expr_block();
                self.eat(&TokenKind::Punctuator(RightBrace))
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
                name: Identifier::from_token(name),
                params: parameters,
                ret: ret_type,
                body,
            },
        })
    }
}
