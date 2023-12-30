#[derive(Debug, Clone)]
pub struct Error {
    pub msg: String,
}
impl Error {
    pub fn new(msg: String) -> Self {
        Self { msg }
    }
}

impl Error {
    pub fn unexpected_eof() -> Self {
        Self::new("Unexpected end of file".to_string())
    }
}