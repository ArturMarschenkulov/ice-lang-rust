#[derive(Debug)]
pub struct LexerError {
    pub msg: String,
}
impl LexerError {
    pub fn new(msg: String) -> LexerError {
        LexerError { msg }
    }
}
