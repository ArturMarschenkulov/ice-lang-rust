#[derive(Debug)]
pub struct ParserError {
    pub msg: String,
}
impl ParserError {
    pub fn new(msg: String) -> Self {
        Self { msg }
    }
}