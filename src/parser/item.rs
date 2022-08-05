use super::{PResult, Parser};
use crate::ast::{Field, Identifier, Item, ItemKind, Parameter, Stmt, StmtKind, Ty, TyKind};
use crate::token::*;

/// This impl block is for parsing items.
impl Parser {
    pub fn parse_item_type_struct(&mut self) -> PResult<Item> {
        use KeywordKind::*;
        use PunctuatorKind::*;
        //use TokenKind::*;

        self.eat(&TokenKind::Keyword(Type)).unwrap();
        let name = self.parse_identifier();

        let _eq_sign = self.eat(&TokenKind::Punctuator(Equal)).unwrap();

        self.eat(&TokenKind::Keyword(Struct)).unwrap();
        self.eat(&TokenKind::Punctuator(LeftBrace)).unwrap();

        let fields = self
            .parse_delim_seq(
                &TokenKind::Punctuator(LeftBrace),
                &TokenKind::Punctuator(RightBrace),
                &TokenKind::Punctuator(Comma),
                ("field", "name", "type"),
            )
            .unwrap();
        let fields = fields
            .iter()
            .map(|(name, ty)| Field {
                name: name.clone(),
                ty: ty.clone(),
            })
            .collect::<Vec<_>>();

        let strct = ItemKind::Struct { name: name, fields };
        let item = Item { kind: strct };
        Ok(item)
    }
    pub fn parse_item_type(&mut self) -> PResult<Item> {
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
    pub fn parse_item(&mut self) -> PResult<Item> {
        use KeywordKind::*;
        use TokenKind::*;

        let item = match &self.peek(0).unwrap().kind {
            Keyword(Fn) => self.parse_item_fn(),
            Keyword(Type) => self.parse_item_type(),
            _ => panic!("unexpected token"),
        };
        item
    }

    /// There 3 variable declaration types.
    /// 1. var <ident> := <expr>;
    /// 2. var <ident> : <type> = <expr>;
    /// 3. var <ident> : <type>;
    pub fn parse_stmt_var(&mut self) -> PResult<Stmt> {
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
            Ok(_) => Some(Box::new(self.parse_expr().unwrap())),
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

        let stmt = Stmt { kind: var_decl };
        Ok(stmt)
    }
    fn parse_delim_seq(
        &mut self,
        start: &TokenKind,
        end: &TokenKind,
        sep: &TokenKind,
        s: (&str, &str, &str),
    ) -> PResult<Vec<(Identifier, Ty)>> {
        use PunctuatorKind::*;
        //use TokenKind::*;

        let mut result = Vec::new();
        self.eat(start).unwrap();
        while self.check(end, 0).is_err() {
            let name = self
                .eat_identifier()
                .unwrap_or_else(|_| panic!("Expected {} {}", s.0, s.1))
                .clone();
            let name = Identifier::from_token(name);
            let _ = self
                .eat(&TokenKind::Punctuator(Colon))
                .unwrap_or_else(|_| panic!("Expected ':' after {} {}", s.0, s.1));
            let ty = self.parse_ty().unwrap();

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
        Ok(result)
    }
    pub fn parse_item_fn(&mut self) -> PResult<Item> {
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
            .unwrap()
            .iter()
            .map(|(name, ty)| Parameter {
                name: name.clone(),
                ty: ty.clone(),
            })
            .collect::<Vec<Parameter>>();

        let ret_type = self
            .eat(&TokenKind::Punctuator(Colon))
            .ok()
            .cloned()
            .map(|_x| self.parse_ty().unwrap());

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
            .unwrap()
            .unwrap();

        // self.check(&Punctuator(LeftBrace), 0)
        //     .expect("Expected '{' after function name");
        // let block = self.parse_expr_block();
        // self.eat(&Punctuator(RightBrace))
        //     .expect("Expected '}' after function body");
        let item = Item {
            kind: ItemKind::Fn {
                name: Identifier::from_token(name),
                params: parameters,
                ret: ret_type,
                body: Box::new(body),
            },
        };
        Ok(item)
    }
}
