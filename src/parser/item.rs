use super::super::lexer::token::{KeywordKind as KK, PunctuatorKind as PK, TokenKind as TK};
use super::ast::{Field, Identifier, Item, Parameter, Stmt, Ty};

use super::{Error, PResult, Parser};

/// This impl block is for parsing items.
impl Parser {
    pub fn parse_item_type_struct(&mut self) -> PResult<Item> {
        self.eat(&TK::Keyword(KK::Type)).unwrap();
        let name = self.parse_identifier().unwrap();

        let _ = self.eat(&TK::Punctuator(PK::Colon)).unwrap();

        self.eat(&TK::Keyword(KK::Struct)).unwrap();
        self.eat(&TK::Punctuator(PK::LeftBrace)).unwrap();

        let fields = self
            .parse_delim_seq(
                &TK::Punctuator(PK::LeftBrace),
                &TK::Punctuator(PK::RightBrace),
                &TK::Punctuator(PK::Comma),
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

        let item = Item::struct_(name, fields);
        Ok(item)
    }
    pub fn parse_item_type(&mut self) -> PResult<Item> {
        // NOTE: Right now no tokens are consumed in this function
        //       This should be delegated for now to other functions.
        //       This function only finds one to which function to dispatch to

        let _ = self.check(&TK::Keyword(KK::Type), 0).unwrap();
        let _ = self.check_with(1, TK::is_identifier).unwrap();
        let _ = self.check(&TK::Punctuator(PK::Colon), 2).unwrap();

        match self.peek(3).unwrap().kind {
            TK::Keyword(KK::Struct) => self.parse_item_type_struct(),
            _ => todo!(),
        }
    }
    pub fn parse_item(&mut self) -> PResult<Item> {
        let item = match &self.peek(0).unwrap().kind {
            TK::Keyword(KK::Fn) => self.parse_item_fn(),
            TK::Keyword(KK::Type) => self.parse_item_type(),
            ut => panic!("unexpected token {:?}", ut),
        };
        item
    }

    /// Parse a variable declaration statement.
    ///
    /// There 3 variable declaration types.
    /// 1. var <ident> : <type> = <expr>;
    /// 2. var <ident> := <expr>;
    /// 3. var <ident> : <type>;
    /// 4. var <ident>; ???
    pub fn parse_stmt_var(&mut self) -> PResult<Stmt> {
        self.eat(&TK::Keyword(KK::Var))
            .expect("guaranteed if called correctly");
        let ident = self.parse_identifier()?;
        self.eat(&TK::Punctuator(PK::Colon)).unwrap();

        let ty = self.parse_ty().ok();

        let expr = match self.eat(&TK::Punctuator(PK::Equal)) {
            Ok(_) => Some(Box::new(self.parse_expr()?)),
            Err(_) if ty.is_some() => None,
            Err(_) => return Err(Error::new(format!("Expected '=', found '{}'", "EOF"))),
        };

        let stmt = Stmt::var(ident, ty, expr);

        self.check(&TK::Punctuator(PK::Semicolon), 0)
            .map_err(|k| match k {
                Some(it) => format!("Expected ';', found '{}'", it.kind.as_str()),
                None => format!("Expected ';', found {:?}", "EOF"),
            })
            .map_err(Error::new)?;

        Ok(stmt)
    }
    fn parse_delim_seq(
        &mut self,
        start: &TK,
        end: &TK,
        sep: &TK,
        s: (&str, &str, &str),
    ) -> PResult<Vec<(Identifier, Ty)>> {
        let mut result = Vec::new();
        self.eat(start).unwrap();
        while self.check(end, 0).is_err() {
            let name = self
                .parse_identifier()
                .map_err(|_| Error::new(format!("Expected {} {}", s.0, s.1)))?;

            self.eat(&TK::Punctuator(PK::Colon))
                .map_err(|_| Error::new(format!("Expected ':' after {} {}", s.0, s.1)))?;

            let ty = self.parse_ty().unwrap();

            match self.eat(sep) {
                Ok(..) => (),
                Err(..) => {
                    let _ = self.check(end, 0).map_err(|_| {
                        Error::new(format!("Expected '{}' after {} {}", end.as_str(), s.0, s.1))
                    });
                }
            }
            result.push((name, ty));
        }
        self.eat(end).map_err(|_| {
            Error::new(format!(
                "Expected '{}' after {} parameters",
                end.as_str(),
                s.0
            ))
        })?;
        Ok(result)
    }
    // fn parse_delim_seq_(
    //     &mut self,
    //     start: &TokenKind,
    //     end: &TokenKind,
    //     sep: &TokenKind,
    //     s: (&str, &str, &str),
    // ) -> PResult<Vec<(Identifier, Ty)>> {
    //     use PunctuatorKind::*;
    //     //use TokenKind::*;

    //     let mut result = Vec::new();
    //     self.eat(start).unwrap();
    //     while self.check(end, 0).is_err() {
    //         let name = self
    //             .parse_identifier()
    //             .unwrap_or_else(|_| panic!("Expected {} {}", s.0, s.1));

    //         self.eat(&TokenKind::Punctuator(Colon))
    //             .unwrap_or_else(|_| panic!("Expected ':' after {} {}", s.0, s.1));
    //         let ty = self.parse_ty().unwrap();

    //         match self.eat(sep) {
    //             Ok(..) => (),
    //             Err(..) => {
    //                 let _ = self.check(end, 0).unwrap_or_else(|_| {
    //                     panic!("Expected '{}' after {} {}", end.as_str(), s.0, s.1)
    //                 });
    //             }
    //         }
    //         result.push((name, ty));
    //     }
    //     self.eat(end)
    //         .unwrap_or_else(|_| panic!("Expected '{}' after {} parameters", end.as_str(), s.0));
    //     Ok(result)
    // }
    pub fn parse_item_fn(&mut self) -> PResult<Item> {
        self.eat(&TK::Keyword(KK::Fn)).unwrap();

        let name = self.parse_identifier().expect("Expected function name");

        let parameters = self
            .parse_delim_seq(
                &TK::Punctuator(PK::LeftParen),
                &TK::Punctuator(PK::RightParen),
                &TK::Punctuator(PK::Comma),
                ("parameter", "name", "type"),
            )
            .unwrap()
            .iter()
            .map(|(name, ty)| Parameter {
                name: name.clone(),
                ty: ty.clone(),
            })
            .collect::<Vec<Parameter>>();

        let ret_type = if self.eat(&TK::Punctuator(PK::Colon)).is_ok() {
            Some(self.parse_ty().unwrap())
        } else {
            None
        };

        self.check(&TK::Punctuator(PK::LeftBrace), 0)
            .ok()
            .or_else(|| {
                panic!("{}", "Expected '{' after function name");
            });

        let body = Box::new(self.parse_expr_block().unwrap());

        self.eat(&TK::Punctuator(PK::RightBrace))
            .expect("Expected '}' after function body");

        let item = Item::fn_(name, parameters, ret_type, *body);
        Ok(item)
    }
}
