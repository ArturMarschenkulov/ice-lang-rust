use crate::token;

#[derive(Debug)]
pub struct LexerError {
    pub msg: String,
}
impl LexerError {
    pub fn new(msg: String) -> Self {
        Self { msg }
    }
}

impl LexerError {
    pub fn unknown_escape_char(c: char) -> Self {
        Self::new(format!("Unknown escape sequence `{}`", c))
    }
    pub fn empty_char_literal() -> Self {
        Self::new("Empty character literal".to_string())
    }
    pub fn new_line_in_char_lit() -> Self {
        Self::new("Newline in character literal".to_string())
    }
    pub fn unterminated_string() -> Self {
        Self::new("Unterminated string".to_string())
    }
    pub fn char_lit_contains_multiple_codepoints() -> Self {
        Self::new("Character literal may only contain one codepoint".to_string())
    }

    pub fn not_recognized_base_prefix(s: &str) -> Self {
        Self::new(format!("'{}' is not a recognized base prefix", s))
    }
    pub fn unknown_character(c: char, pos: token::Position) -> Self {
        Self::new(format!(
            "Unknown character '{}' at {}:{}",
            c, pos.line, pos.column
        ))
    }
    pub fn floats_dont_have_base_prefix(base: Option<token::NumberBase>) -> Self {
        LexerError::new(format!(
            "floating point literals are not allowed to have a base prefix (`{}`)",
            base.unwrap_or(token::NumberBase::Decimal).as_num()
        ))
    }
    pub fn invalid_digit_base_prefix(base: Option<token::NumberBase>) -> Self {
        LexerError::new(format!(
            "invalid digit for base prefix `{}`",
            base.unwrap_or(token::NumberBase::Decimal).as_num()
        ))
    }
    pub fn invalid_digit_type_suffix(suffix: Option<String>) -> Self {
        LexerError::new(format!(
            "invalid suffix `{}` for numeric literal",
            suffix.unwrap_or_default()
        ))
    }
}

#[derive(Debug)]
pub struct ParserError {
    pub msg: String,
}
impl ParserError {
    pub fn new(msg: String) -> Self {
        Self { msg }
    }
}
