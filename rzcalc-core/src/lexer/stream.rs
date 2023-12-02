use std::{iter::Peekable, rc::Rc, str::Chars};

pub struct CharStream<'s> {
    pub(super) row: usize,
    pub(super) col: usize,
    pub(super) index: usize,
    pub(super) content: String,
    pub(super) iter: Peekable<Chars<'s>>,
}

impl CharStream<'_> {
    pub const EOF: char = '\0';

    pub fn new<'s>(content: &'s str) -> CharStream<'s> {
        let iter = content.chars().peekable();
        let content = content.to_owned();
        CharStream::<'s> {
            row: 0,
            col: 0,
            index: 0,
            content,
            iter,
        }
    }

    pub fn from_bytes<'s>(content: &'s [u8]) -> Result<CharStream<'s>, crate::RzError> {
        let content = std::str::from_utf8(content)?;
        Ok(CharStream::<'s>::new(content))
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

    #[inline]
    pub fn col(&self) -> usize {
        self.col
    }
    #[inline]
    pub fn row(&self) -> usize {
        self.row
    }

    #[inline]
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
impl Iterator for CharStream<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.gnext()
    }
}
