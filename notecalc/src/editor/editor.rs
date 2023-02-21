use std::fmt::Debug;
use std::ops::{Range, RangeInclusive};

pub const EDITOR_CURSOR_TICK_MS: u32 = 500;
pub const TAB_SIZE: usize = 4;
pub const SPACES: [&str; 4] = [" ", "  ", "   ", "    "];

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub enum EditorInputEvent {
    Left,
    Right,
    Up,
    Down,
    Home,
    End,
    Esc,
    PageUp,
    PageDown,
    Enter,
    Backspace,
    Del,
    Tab,
    Char(char),
}

#[repr(C)]
#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub struct InputModifiers {
    pub shift: bool,
    pub ctrl: bool,
    pub alt: bool,
}

impl InputModifiers {
    pub fn none() -> InputModifiers {
        InputModifiers {
            shift: false,
            ctrl: false,
            alt: false,
        }
    }

    pub fn ctrl() -> InputModifiers {
        InputModifiers {
            shift: false,
            ctrl: true,
            alt: false,
        }
    }

    pub fn alt() -> InputModifiers {
        InputModifiers {
            shift: false,
            ctrl: false,
            alt: true,
        }
    }

    pub fn shift() -> InputModifiers {
        InputModifiers {
            shift: true,
            ctrl: false,
            alt: false,
        }
    }

    pub fn ctrl_shift() -> InputModifiers {
        InputModifiers {
            shift: true,
            ctrl: true,
            alt: false,
        }
    }

    pub fn is_ctrl_shift(&self) -> bool {
        self.ctrl && self.shift
    }

    pub fn is_none(&self) -> bool {
        !self.ctrl && !self.shift && !self.alt
    }
}

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

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub struct Selection {
    pub start: Pos,
    pub end: Option<Pos>,
}

impl Selection {
    pub fn single(pos: Pos) -> Selection {
        Selection {
            start: pos,
            end: None,
        }
    }

    pub fn single_r_c(row_index: usize, column_index: usize) -> Selection {
        Selection {
            start: Pos {
                row: row_index,
                column: column_index,
            },
            end: None,
        }
    }

    pub fn range(start: Pos, end: Pos) -> Selection {
        Selection {
            start,
            end: if start == end { None } else { Some(end) },
        }
    }

    pub fn get_range_ordered(&self) -> (Pos, Pos) {
        if let Some(end) = self.end {
            let end_index = end.row * 1024 + end.column;
            let start_index = self.start.row * 1024 + self.start.column;
            if end_index < start_index {
                (end, self.start)
            } else {
                (self.start, end)
            }
        } else {
            (self.start, self.start)
        }
    }

    pub fn get_range(&self) -> (Pos, Pos) {
        if let Some(end) = self.end {
            (self.start, end)
        } else {
            (self.start, self.start)
        }
    }

    pub fn is_range(&self) -> bool {
        self.end.is_some()
    }

    pub fn is_range_unordered(&self) -> Option<(Pos, Pos)> {
        if let Some(end) = self.end {
            Some((self.start, end))
        } else {
            None
        }
    }

    pub fn is_range_ordered(&self) -> Option<(Pos, Pos)> {
        if let Some(end) = self.end {
            let end_index = end.row * 1024 + end.column;
            let start_index = self.start.row * 1024 + self.start.column;
            if end_index < start_index {
                Some((end, self.start))
            } else {
                Some((self.start, end))
            }
        } else {
            None
        }
    }

    pub fn get_first(&self) -> Pos {
        if let Some(end) = self.end {
            let end_index = end.row * 1024 + end.column;
            let start_index = self.start.row * 1024 + self.start.column;
            if end_index < start_index {
                end
            } else {
                self.start
            }
        } else {
            self.start
        }
    }

    pub fn get_second(&self) -> Pos {
        if let Some(end) = self.end {
            let end_index = end.row * 1024 + end.column;
            let start_index = self.start.row * 1024 + self.start.column;
            if end_index > start_index {
                end
            } else {
                self.start
            }
        } else {
            self.start
        }
    }

    pub fn extend(&self, new_end: Pos) -> Selection {
        return if self.start == new_end {
            Selection::single_r_c(new_end.row, new_end.column)
        } else {
            Selection::range(self.start, new_end)
        };
    }

    pub fn get_cursor_pos(&self) -> Pos {
        self.end.unwrap_or(self.start)
    }

    pub fn get_row_iter_incl(&self) -> RangeInclusive<usize> {
        let start = self.get_first().row;
        let end = self.get_second().row;
        start..=end
    }

    pub fn get_row_iter_excl(&self) -> Range<usize> {
        let start = self.get_first().row;
        let end = self.get_second().row;
        start..end
    }

    pub fn clone_with_start(&self, start: Pos) -> Selection {
        Selection {
            start,
            end: self.end,
        }
    }

    pub fn clone_with_end(&self, end: Pos) -> Selection {
        Selection {
            start: self.start,
            end: Some(end),
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

#[derive(Debug)]
struct CursorData {
    selection: Selection, // Vec when multicursor
    last_column_index: usize,
}

impl CursorData {
    #[inline]
    #[allow(dead_code)]
    pub fn set_cursor_pos(&mut self, pos: Pos) {
        self.set_selection_save_col(Selection::single(pos));
    }

    #[inline]
    #[allow(dead_code)]
    pub fn set_cursor_pos_r_c(&mut self, row_index: usize, column_index: usize) {
        self.set_selection_save_col(Selection::single_r_c(row_index, column_index));
    }

    #[inline]
    #[allow(dead_code)]
    pub fn set_cursor_range(&mut self, start: Pos, end: Pos) {
        self.set_selection_save_col(Selection::range(start, end));
    }

    #[inline]
    pub fn set_selection_save_col(&mut self, selection: Selection) {
        self.selection = selection;
        self.last_column_index = selection.get_cursor_pos().column;
        debug_assert!(self.last_column_index <= 120, "{}", self.last_column_index);
    }
}
