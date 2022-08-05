#[derive(Debug)]
pub struct LexerError {
    pub msg: String,
}
impl LexerError {
    pub fn new(msg: String) -> Self {
        Self { msg }
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
