use super::super::lexer::token::{KeywordKind as KK, PunctuatorKind as PK, TokenKind as TK};
use super::ast::{Expr, Field, Identifier, Item, Parameter, Stmt, Ty};

use super::{Error, PResult, Parser};

/// This impl block is for parsing items.
impl Parser {
    pub fn parse_item_type_struct(&mut self) -> PResult<Item> {
        self.eat(&TK::Keyword(KK::Type)).unwrap();
        let name = self.parse_identifier().unwrap();

        let _ = self.eat(&TK::Punctuator(PK::Colon)).unwrap();

        self.eat(&TK::Keyword(KK::Struct)).unwrap();

        self.check(&TK::Punctuator(PK::LeftBrace), 0).unwrap();
        let fields = self
            .parse_delim_seq_ident_and_type(
                &TK::Punctuator(PK::LeftBrace),
                &TK::Punctuator(PK::RightBrace),
                &TK::Punctuator(PK::Comma),
            )
            .unwrap()
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
    pub fn parse_item_fn(&mut self) -> PResult<Item> {
        self.eat(&TK::Keyword(KK::Fn)).unwrap();

        let name = self.parse_identifier().expect("Expected function name");

        let parameters = self
            .parse_delim_seq_ident_and_type(
                &TK::Punctuator(PK::LeftParen),
                &TK::Punctuator(PK::RightParen),
                &TK::Punctuator(PK::Comma),
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

        let item = Item::fn_(name, parameters, ret_type, *body);
        Ok(item)
    }
}

#[derive(Debug)]
pub enum DelimError {
    /// The start token kind was not found, instead this one.
    Start(TK),
    /// The end token kind was not found, instead this one.
    End(TK),
    /// The separator token kind was not found, instead this one.
    Sep(TK),
    /// An error occured while parsing the inner item.
    Inner(Error),
}

/* The delim parsing needs to be more generic to reduce code duplication.

*/
// The different types of delim sequences
// (a, b, c): simply a list of items. Those items are however expressions and thus could be arbitrarily complex.
// (a: b, c: d): a list of items, where each item is a pair of an identifier and a type.

impl Parser {
    /// Parses a delim sequences.
    ///
    /// A delim sequence is a sequence of items, where each item is separated by a separator.
    /// The sequence is surrounded by a start and end token.
    ///
    /// NOTE: When `sep == None`, it amounts to being separated by whitespace.
    pub fn parse_delim_seq<T>(
        &mut self,
        start: &TK,
        end: &TK,
        sep: Option<&TK>,
        parser: impl Fn(&mut Parser) -> PResult<T>,
    ) -> Result<Vec<T>, (Vec<T>, DelimError)>
    where
        T: Clone,
    {
        let mut result = Vec::new();
        self.eat(start)
            .map_err(|e| (result.clone(), DelimError::Start(e.unwrap().kind.clone())))?;
        while self.check(end, 0).is_err() {
            let item = parser(self).map_err(|e| (result.clone(), DelimError::Inner(e)))?;
            result.push(item);

            if let Some(sep) = sep {
                match self.eat(sep) {
                    Ok(..) => (),
                    Err(None) => panic!("Expected separator, got unexpected EOF"),
                    Err(Some(token_instead)) => {
                        if token_instead.kind == *end {
                            break;
                        } else {
                            return Err((result, DelimError::Sep(token_instead.kind.clone())));
                        }
                    }
                }
            }
        }
        self.eat(end)
            .map_err(|e| (result.clone(), DelimError::End(e.unwrap().kind.clone())))?;
        Ok(result)
    }
    pub fn parse_ident_and_type(this: &mut Parser) -> PResult<(Identifier, Ty)> {
        let name = this.parse_identifier().unwrap();
        this.eat(&TK::Punctuator(PK::Colon)).unwrap();
        let ty = this.parse_ty().unwrap();

        Ok((name, ty))
    }
    fn parse_delim_seq_ident_and_type(
        &mut self,
        start: &TK,
        end: &TK,
        sep: &TK,
    ) -> Result<Vec<(Identifier, Ty)>, (Vec<(Identifier, Ty)>, DelimError)> {
        self.parse_delim_seq(start, end, Some(sep), Parser::parse_ident_and_type)
    }

    pub fn parse_delim_seq_expr(
        &mut self,
        start: &TK,
        end: &TK,
        sep: &TK,
    ) -> Result<Vec<Expr>, (Vec<Expr>, DelimError)> {
        fn parse_expr(this: &mut Parser) -> PResult<Expr> {
            this.parse_expr()
        }
        self.parse_delim_seq(start, end, Some(sep), parse_expr)
    }
}

mod tests {
    use super::super::super::lexer;
    use super::*;
    #[test]
    fn parse_delim_seq() {
        fn get_tokens(txt: &str) -> Vec<lexer::token::Token> {
            lexer::lex_tokens_from_file(txt).unwrap().tokens
        }
        use TK::Punctuator as TKP;
        const LEFT_PAREN: TK = TKP(PK::LeftParen);
        const RIGHT_PAREN: TK = TKP(PK::RightParen);
        const LEFT_BRACE: TK = TKP(PK::LeftBrace);
        const RIGHT_BRACE: TK = TKP(PK::RightBrace);
        const LEFT_BRACKET: TK = TKP(PK::LeftBracket);
        const RIGHT_BRACKET: TK = TKP(PK::RightBracket);
        const VERTICAL_BAR: TK = TKP(PK::VerticalBar);

        const COMMA: TK = TKP(PK::Comma);
        const SEMICOLON: TK = TKP(PK::Semicolon);
        const COLON: TK = TKP(PK::Colon);

        let txt = "(a)";
        Parser::new(get_tokens(txt))
            .parse_delim_seq(&LEFT_PAREN, &RIGHT_PAREN, Some(&COMMA), |p| {
                p.parse_identifier()
            })
            .unwrap();

        let txt = "(a,)";
        Parser::new(get_tokens(txt))
            .parse_delim_seq(&LEFT_PAREN, &RIGHT_PAREN, Some(&COMMA), |p| {
                p.parse_identifier()
            })
            .unwrap();

        let txt = "(a, b, c)";
        Parser::new(get_tokens(txt))
            .parse_delim_seq(&LEFT_PAREN, &RIGHT_PAREN, Some(&COMMA), |p| {
                p.parse_identifier()
            })
            .unwrap();

        let txt = "(a, b, c,)";
        Parser::new(get_tokens(txt))
            .parse_delim_seq(&LEFT_PAREN, &RIGHT_PAREN, Some(&COMMA), |p| {
                p.parse_identifier()
            })
            .unwrap();

        let txt = "(a b c)";
        Parser::new(get_tokens(txt))
            .parse_delim_seq(&LEFT_PAREN, &RIGHT_PAREN, None, |p| p.parse_identifier())
            .unwrap();

        // let txt = "(a; b; c,)";
        // Parser::new(get_tokens(txt))
        //     .parse_delim_seq(&LEFT_PAREN, &RIGHT_PAREN, Some(&SEMICOLON), |p| {
        //         p.parse_identifier()
        //     })
        //     .unwrap();
    }
}
