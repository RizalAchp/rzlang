use std::{iter::Peekable, rc::Rc, str::Chars};

pub struct CharStream<'s> {
    pub(super) row: usize,
    pub(super) col: usize,
    pub(super) index: usize,
    pub(super) content: &'s str,
    pub(super) iter: Peekable<Chars<'s>>,
}

impl CharStream<'_> {
    pub const EOF: char = '\0';

    pub fn new<'s>(string: &'s str) -> CharStream<'s> {
        CharStream::<'s> {
            row: 0,
            col: 0,
            index: 0,
            content: string,
            iter: string.chars().peekable(),
        }
    }

    #[inline]
    pub fn gpeek(&mut self) -> Option<char> {
        self.iter.peek().copied()
    }

    #[inline]
    pub fn gnext(&mut self) -> Option<char> {
        self.index += 1;
        match self.iter.next() {
            Some(ok) => match ok {
                '\n' => {
                    self.row += 1;
                    self.col = 0;
                    Some(ok)
                }
                _ => {
                    self.col += 1;
                    Some(ok)
                }
            },
            None => None,
        }
    }

    pub fn col(&self) -> usize {
        self.col
    }
    pub fn row(&self) -> usize {
        self.row
    }
    pub fn current_line(&self) -> Rc<str> {
        let bol = self.index.saturating_sub(self.col);
        match self.content.get((bol + 1)..).and_then(|x| x.find('\n')) {
            Some(eol) => Rc::from(&self.content[bol..eol]),
            None => Rc::from(&self.content[bol..]),
        }
    }

    #[inline]
    pub fn next_if<F>(&mut self, pred: F) -> Option<char>
    where
        F: FnOnce(char) -> bool,
    {
        if pred(self.gpeek()?) {
            self.gnext()
        } else {
            None
        }
    }

    #[inline]
    pub fn peek_char(&mut self) -> char {
        self.gpeek().unwrap_or(Self::EOF)
    }
    #[inline]
    pub fn next_char(&mut self) -> char {
        self.gnext().unwrap_or(Self::EOF)
    }
}
