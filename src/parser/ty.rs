use super::{PResult, Parser};
use crate::ast::{Identifier, Ty, TyKind};
use crate::error::ParserError;
use crate::token::{TokenKind, PunctuatorKind};

/// This impl block is for parsing the types
impl Parser {
    pub fn parse_ty(&mut self) -> PResult<Ty> {
        let token = self.peek(0).unwrap();
        let ty = match &token.kind {
            TokenKind::Identifier(..) => {
                let ty = Ty {
                    kind: TyKind::Simple(Identifier::from(token.clone())),
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
