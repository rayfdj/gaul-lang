#[derive(Debug, Clone, Copy, Default)]
pub struct Span {
    pub line: usize,   // 1-indexed
    pub col: usize,    // 1-indexed, char offset within line
    pub length: usize, // token length in chars (for ^^^ underline)
}
