use std::{iter::Peekable, str::Chars};

pub struct CharStream<'s> {
    pub(super) row: usize,
    pub(super) col: usize,
    pub(super) index: isize,
    pub(super) content: &'s str,
    pub(super) iter: Peekable<Chars<'s>>,
}

impl<'s> CharStream<'s> {
    pub const EOF: char = '\0';

    pub fn new(content: &'s str) -> Self {
        let iter = content.chars().peekable();
        CharStream::<'s> {
            row: 0,
            col: 0,
            index: -1,
            content,
            iter,
        }
    }

    #[inline]
    pub fn gnext(&mut self, c: char) -> Option<char> {
        self.index += 1;
        match c {
            '\n' => {
                self.row += 1;
                self.col = 0;
                Some(c)
            }
            _ => {
                self.col += 1;
                Some(c)
            }
        }
    }

    #[inline]
    pub fn col(&self) -> usize {
        self.col
    }
    #[inline]
    pub fn row(&self) -> usize {
        self.row
    }

    #[inline(always)]
    pub fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    pub fn get_string_when(&mut self, pred: impl FnOnce(&char) -> bool + Copy) -> &'s str {
        let begin = self.index as usize;
        while self
            .iter
            .next_if(pred)
            .and_then(|x| self.gnext(x))
            .is_some()
        {}
        &self.content[begin..=(self.index as usize)]
    }
}
impl Iterator for CharStream<'_> {
    type Item = char;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().and_then(|x| self.gnext(x))
    }
}
