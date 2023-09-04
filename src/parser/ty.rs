use super::super::lexer::token::{PunctuatorKind, TokenKind};
use super::ast::{Identifier, Ty};

use super::error::*;
use super::{PResult, Parser};

/// This impl block is for parsing the typess
impl Parser {
    pub fn parse_ty(&mut self) -> PResult<Ty> {
        use PunctuatorKind as PK;
        use TokenKind as TK;
        let token = self.peek(0).unwrap().clone();
        let ty = match &token.kind {
            TK::Identifier(..) => {
                self.advance();
                Ty::simple(Identifier::try_from(token).unwrap())
            }
            TK::Punctuator(PK::LeftParen) => {
                self.advance();
                let _ = self.eat(&TK::Punctuator(PK::RightParen)).unwrap();
                Ty::unit()
            }
            _ => Err(Error::new(format!("Expected a type, got {:?}", token)))?,
        };
        Ok(ty)
    }
}
