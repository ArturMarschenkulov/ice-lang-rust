#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}
impl Position {
    pub fn new(line: u32, column: u32) -> Position {
        Position { line, column }
    }
}
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}
impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Span { start, end }
    }
}
impl From<(Span, Span)> for Span {
    fn from(spans: (Span, Span)) -> Self {
        Span {
            start: spans.0.start,
            end: spans.1.end,
        }
    }
}
impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "Span {{{}:{}-{}:{}}}",
            self.start.line, self.start.column, self.end.line, self.end.column
        )
    }
}
impl From<((u32, u32), (u32, u32))> for Span {
    fn from(tuples: ((u32, u32), (u32, u32))) -> Self {
        Span {
            start: Position::new(tuples.0 .0, tuples.0 .1),
            end: Position::new(tuples.1 .0, tuples.1 .1),
        }
    }
}

/// NOTE: To be used in the future
struct SourceMap {
    // files: HashMap<String, FileId>,
}