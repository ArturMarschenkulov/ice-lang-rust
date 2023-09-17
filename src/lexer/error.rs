use super::{span, token};

#[derive(Debug, PartialEq, Eq, Clone)]
enum ErrorKind {
    // char lit errors
    /// Empty character literal
    EmptyCharLiteral,
    /// Character literal may only contain one codepoint
    CharLitContainsMultipleCodepoints,
    /// Unterminated character literal.
    ///
    /// NOTE: Unrecoverable.
    UnterminatedCharLiteral,
    /// Unknown escape sequence
    UnknownEscapeChar(char),
    /// Newline in character literal
    NewlineInCharLit,

    // string lit errors
    /// Unterminated string
    ///
    /// NOTE: Unrecoverable.
    UnterminatedString,

    // comment errors
    /// Unterminated block comment
    ///
    /// NOTE: Unrecoverable.
    UnterminatedBlockComment,

    // number errors
    NotRecognizedBasePrefix(String),
    /// Invalid prefix for numeric literal
    ///
    /// NOTE: Recoverable.
    FloatsDontHaveBasePrefix(Option<token::NumberBase>),
    /// Invalid digit for base prefix
    ///
    /// NOTE: Recoverable.
    InvalidDigitBasePrefix(Option<token::NumberBase>),
    InvalidDigitTypeSuffix(Option<String>),

    /// NOTE: This error is mostly a placeholder. In the end, no error should be of this kind.
    Generic(String),
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Error {
    kind: ErrorKind,
}
impl Error {
    pub fn new(msg: String) -> Self {
        Self {
            kind: ErrorKind::Generic(msg),
        }
    }
}

impl Error {
    pub fn expected_char(expected: char, found: Option<char>) -> Self {
        Self::new(format!("Expected '{}' but found '{:?}'", expected, found))
    }
    // pub fn expected_str(expected: &str, found: Option<&str>) -> Self {
    //     Self::new(format!("Expected '{}' but found '{:?}'", expected, found))
    // }
    // mainly about chars
    pub fn empty_char_literal() -> Self {
        // Self::new("Empty character literal".to_string())
        Self {
            kind: ErrorKind::EmptyCharLiteral,
        }
    }
    pub fn char_lit_contains_multiple_codepoints() -> Self {
        // Self::new("Character literal may only contain one codepoint".to_string())
        Self {
            kind: ErrorKind::CharLitContainsMultipleCodepoints,
        }
    }

    pub fn unterminated_char_lit() -> Self {
        // Self::new("Unterminated character literal".to_string())
        Self {
            kind: ErrorKind::UnterminatedCharLiteral,
        }
    }

    pub fn unknown_escape_char(c: char) -> Self {
        // Self::new(format!("Unknown escape sequence `{}`", c))
        Self {
            kind: ErrorKind::UnknownEscapeChar(c),
        }
    }
    pub fn new_line_in_char_lit() -> Self {
        // Self::new("Newline in character literal".to_string())
        Self {
            kind: ErrorKind::NewlineInCharLit,
        }
    }

    // mainly about strings
    pub fn unterminated_string() -> Self {
        // Self::new("Unterminated string".to_string())
        Self {
            kind: ErrorKind::UnterminatedString,
        }
    }

    // mainly about comments
    pub fn unterminated_block_comment() -> Self {
        // Self::new("Unterminated block comment".to_string())
        Self {
            kind: ErrorKind::UnterminatedBlockComment,
        }
    }

    // mainly about numbers
    pub fn not_recognized_base_prefix(s: String) -> Self {
        // Self::new(format!("'{}' is not a recognized base prefix", s))
        Self {
            kind: ErrorKind::NotRecognizedBasePrefix(s),
        }
    }

    pub fn floats_dont_have_base_prefix(base: Option<token::NumberBase>) -> Self {
        // Self::new(format!(
        //     "floating point literals are not allowed to have a base prefix (`{}`)",
        //     u32::from(base.unwrap_or(token::NumberBase::Decimal))
        // ))
        Self {
            kind: ErrorKind::FloatsDontHaveBasePrefix(base),
        }
    }
    pub fn invalid_digit_base_prefix(base: Option<token::NumberBase>) -> Self {
        // Self::new(format!(
        //     "invalid digit for base prefix `{}`",
        //     u32::from(base.unwrap_or(token::NumberBase::Decimal))
        // ))
        Self {
            kind: ErrorKind::InvalidDigitBasePrefix(base),
        }
    }
    pub fn invalid_digit_type_suffix(suffix: String) -> Self {
        // Self::new(format!("invalid suffix `{}` for numeric literal", suffix))
        Self {
            kind: ErrorKind::InvalidDigitTypeSuffix(Some(suffix)),
        }
    }
    // General
    pub fn unknown_character(c: char, pos: span::Position) -> Self {
        Self::new(format!(
            "Unknown character '{}' at {}:{}",
            c, pos.line, pos.column
        ))
    }
}
