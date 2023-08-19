use crate::ast::{Identifier, Ty, TyKind};
use crate::token::{PunctuatorKind, TokenKind};

use super::error::*;
use super::{PResult, Parser};

/// This impl block is for parsing the typess
impl Parser {
    pub fn parse_ty(&mut self) -> PResult<Ty> {
        let token = self.peek(0).unwrap();
        let ty = match &token.kind {
            TokenKind::Identifier(..) => {
                let ty = Ty {
                    kind: TyKind::Simple(Identifier::try_from(token.clone()).unwrap()),
                };

                self.advance();
                ty
            }
            TokenKind::Punctuator(PunctuatorKind::LeftParen) => {
                self.advance();
                let _ = self
                    .eat(&TokenKind::Punctuator(PunctuatorKind::RightParen))
                    .unwrap();
                Ty {
                    kind: TyKind::Tuple(Vec::new()),
                }
            }
            _ => Err(ParserError::new(format!(
                "Expected a type, got {:?}",
                token
            )))?,
        };
        Ok(ty)
    }
}
