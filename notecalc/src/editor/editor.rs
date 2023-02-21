#[derive(Default, Eq, PartialEq, Debug, Clone, Copy)]
pub struct Pos {
    pub row: usize,
    pub column: usize,
}

impl Pos {
    pub fn from_row_column(row_index: usize, column_index: usize) -> Pos {
        Pos {
            row: row_index,
            column: column_index,
        }
    }

    pub fn with_column(&self, col: usize) -> Pos {
        Pos {
            column: col,
            ..*self
        }
    }

    pub fn with_row(&self, row: usize) -> Pos {
        Pos { row, ..*self }
    }

    pub fn add_column(&self, col: usize) -> Pos {
        Pos {
            column: self.column + col,
            ..*self
        }
    }

    pub fn sub_column(&self, col: usize) -> Pos {
        Pos {
            column: self.column - col.min(self.column),
            ..*self
        }
    }

    pub fn with_next_row(&self) -> Pos {
        Pos {
            row: self.row + 1,
            ..*self
        }
    }

    pub fn with_prev_row(&self) -> Pos {
        Pos {
            row: self.row - 1,
            ..*self
        }
    }

    pub fn with_next_col(&self) -> Pos {
        Pos {
            column: self.column + 1,
            ..*self
        }
    }

    pub fn with_prev_col(&self) -> Pos {
        Pos {
            column: self.column - 1,
            ..*self
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RowModificationType {
    SingleLine(usize),
    AllLinesFrom(usize),
}

impl RowModificationType {
    pub fn merge(&mut self, other: Option<&RowModificationType>) {
        let self_row = match self {
            RowModificationType::SingleLine(row) => *row,
            RowModificationType::AllLinesFrom(row) => *row,
        };
        if let Some(other) = other {
            let other_row = match other {
                RowModificationType::SingleLine(row) => row,
                RowModificationType::AllLinesFrom(row) => row,
            };
            *self = match (&self, other) {
                (
                    RowModificationType::SingleLine(self_row),
                    RowModificationType::SingleLine(other_row),
                ) if self_row == other_row => RowModificationType::SingleLine(*self_row),
                _ => RowModificationType::AllLinesFrom(self_row.min(*other_row)),
            };
        }
    }
}

