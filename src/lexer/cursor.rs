//! This module contains the cursor part of the lexer.
//!
//! The cursor is the part of the lexer that keeps track of the current position in the source code
//! It is used to advance the lexer and to peek at the next character.
//!
//! There are 3 main types, `peek`, `check` and `eat`.
//! - Peeking just describes the act of looking without doing anything.
//! - Checking is the act of peeking and then checking whether the peeked character agrees with a given predicate.
//! - Eating is the act of checking and then advancing the cursor if the check was successful.

use super::span;

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

/// NOTE: To be used in the future
pub struct Cursor {
    pub index: usize,
    pub cursor: span::Position,
    pub chars: Vec<char>,
}

impl Cursor {
    pub fn new(chars: Vec<char>) -> Self {
        Self {
            index: 0,
            cursor: span::Position::new(0, 0),
            chars,
        }
    }
}

/// This impl is the cursor part for the lexer.
///
///
impl Cursor {
    /// Advances the cursor by one.
    pub fn advance(&mut self) {
        if let Some(i) = adjust_index_safely(self.index, 1) {
            self.index = i;
            self.cursor.column += 1;
        }
    }

    /// Peeks the character at the current index plus the given offset.
    ///
    /// This method takes an offset as an `isize` and attempts to return the character
    /// at the position that's at `index + offset`. If the calculated index is out of
    /// bounds (either less than zero or greater than or equal to the length of the
    /// characters array), it returns `None`.
    pub fn peek(&self, offset: isize) -> Option<char> {
        adjust_index_safely(self.index, offset).and_then(|index| self.chars.get(index).copied())
    }

    /// Checks whether the character at the given offset agrees with the provided predicate.
    ///
    /// This method takes an offset as an `isize` and a predicate function. It then checks whether the character at the
    ///
    /// Peeks at the offset `offset`. If the peeked char agrees with the given predicate, it returns it wrapped in a `Ok`,
    /// otherwise it returns the peeked char wrapped in a `Err`.
    pub fn check_with<F>(&self, offset: isize, pred: F) -> Result<char, Option<char>>
    where
        Self: Sized,
        F: Fn(&char) -> bool,
    {
        self.peek(offset)
            .map(|c| if pred(&c) { Ok(c) } else { Err(Some(c)) })
            .unwrap_or(Err(None))
    }

    /// Checks the char at offset `n`. If the char is the same as the given char, it returns it wrapped in a `Some`, otherwise `None`.
    pub fn check_char(&self, n: isize, c: char) -> Result<char, Option<char>> {
        self.check_with(n, |x| x == &c)
    }

    /// Matches a terminal character. If the character is matched, it is eaten and return wrapped in a `Ok`,
    /// else an `Err(Some)` is returned if the character is not matched and an `Err(None)` is returned if the end of the file is reached
    pub fn eat_char(&mut self, ch: char) -> Result<char, Option<char>> {
        self.eat_with(|x| x == &ch)
    }

    /// Checks the charachter at the current index with a given function.
    /// If the function returns true, the current char is "eaten" and returned wrapped in a `Some`, otherwise `None`.
    pub fn eat_with<F>(&mut self, pred: F) -> Result<char, Option<char>>
    where
        F: Fn(&char) -> bool,
    {
        self.check_with(0, &pred).map(|char| {
            self.advance();
            char
        })
    }

    pub fn eat_while<F>(&mut self, pred: F) -> String
    where
        F: Fn(&char) -> bool,
    {
        (0..)
            .map(|_| self.eat_with(&pred))
            .take_while(Result::is_ok)
            .filter_map(Result::ok)
            .collect()
    }

    pub fn eat_str(&mut self, str: &str) -> Option<String> {
        str.chars()
            .map(|ch| self.eat_char(ch).ok())
            .collect::<Option<String>>()
    }
}
