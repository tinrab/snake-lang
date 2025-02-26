use std::fmt::{self, Display, Formatter};

/// A span of text, represented by its starting and ending line and column.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Span {
    pub line_start: usize,
    pub line_end: usize,
    pub column_start: usize,
    pub column_end: usize,
}

impl Span {
    pub fn new(line_start: usize, line_end: usize, column_start: usize, column_end: usize) -> Self {
        Self {
            line_start,
            line_end,
            column_start,
            column_end,
        }
    }

    pub fn union(&self, other: &Self) -> Self {
        Self {
            line_start: self.line_start.min(other.line_start),
            line_end: self.line_end.max(other.line_end),
            column_start: self.column_start.min(other.column_start),
            column_end: self.column_end.max(other.column_end),
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}-{}:{}",
            self.line_start, self.column_start, self.line_end, self.column_end
        )
    }
}
