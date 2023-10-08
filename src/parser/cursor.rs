use super::super::lexer::token::{SpecialKeywordKind, Token, TokenKind};
use super::Parser;
/// Adjusts the given index by the given offset safely.
///
/// This function takes an index as a `usize` and an offset as an `isize`. It attempts to add or subtract the offset
/// to/from the index based on the sign of the offset. It ensures that the operation is checked, meaning that it will not
/// cause any overflow or underflow.
fn adjust_index_safely(index: usize, offset: isize) -> Option<usize> {
    match offset.is_positive() {
        true => index.checked_add(offset as usize),
        false => index.checked_sub(-offset as usize),
    }
}

/// This impl block is for the cursor functionality
impl Parser {
    pub fn peek(&self, offset: isize) -> Option<&Token> {
        adjust_index_safely(self.current, offset).and_then(|new_offset| self.tokens.get(new_offset))
    }

    pub fn check(&self, kind: &TokenKind, offset: isize) -> Result<&Token, Option<&Token>> {
        self.check_with(offset, |tk| tk == kind)
    }

    /// Checks if the token at the given offset matches the given predicate.
    /// If it does, it is returned, else `Err` is returned.
    pub fn check_with<F>(&self, offset: isize, pred: F) -> Result<&Token, Option<&Token>>
    where
        Self: Sized,
        F: Fn(&TokenKind) -> bool,
    {
        self.peek(offset)
            .map(|c| if pred(&c.kind) { Ok(c) } else { Err(Some(c)) })
            .unwrap_or(Err(None))
    }
    pub fn eat(&mut self, kind: &TokenKind) -> Result<&Token, Option<&Token>> {
        self.eat_with(|t| t == kind)
    }
    pub fn eat_identifier(&mut self) -> Result<&Token, Option<&Token>> {
        self.eat_with(TokenKind::is_identifier)
    }
    pub fn eat_punctuator(&mut self) -> Result<&Token, Option<&Token>> {
        self.eat_with(TokenKind::is_punctuator)
    }
    pub fn eat_punctuator_with(
        &mut self,
        pred: impl Fn(&TokenKind) -> bool,
    ) -> Result<&Token, Option<&Token>> {
        self.eat_with(|t| TokenKind::is_punctuator(t) && pred(t))
    }

    /// Eats the next token if it matches the given predicate.
    ///
    /// If the next token matches the predicate, it is consumed and returned.
    /// Otherwise, the cursor is not moved and `Err` is ret
    fn eat_with<F>(&mut self, pred: F) -> Result<&Token, Option<&Token>>
    where
        Self: Sized,
        F: Fn(&TokenKind) -> bool,
    {
        // This is an ugly workaround for a limitation of the borrow checker.
        // Rust's new polonius would be able to handle this better,
        // but for now we'll just use this.
        // TODO: If polonius is ready, use the better version.

        if self.check_with(0, &pred).is_ok() {
            self.advance()
        }

        match self.peek(-1) {
            Some(token) if pred(&token.kind) => Ok(token),
            peeked @ Some(..) => Err(peeked),
            None => Err(None),
        }

        // // After polonius is ready, this SHOULD work
        // match self.check_with(0, &pred) {
        //     Ok(token) => {
        //         self.advance();
        //         Ok(token)
        //     }
        //     Err(err) => Err(err),
        // }

        // self.check_with(0, &pred).map(|tok| {
        //     self.advance();
        //     tok
        // })
    }
    pub fn advance(&mut self) {
        if !self.is_at_end() {
            self.current += 1;
        }
    }
    pub fn is_at_end(&self) -> bool {
        use SpecialKeywordKind as SKK;
        use TokenKind as TK;
        self.peek(0).unwrap().kind == TK::SpecialKeyword(SKK::Eof)
    }
}