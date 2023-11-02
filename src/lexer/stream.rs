use std::{
    fs::File,
    io::{BufReader, Bytes, Read},
    iter::Peekable,
    path::Path,
    str::Bytes as StrBytes,
};

pub trait Stream {
    const EOF: u8 = b'\0';

    fn index(&self) -> usize;
    fn peek(&mut self) -> Option<u8>;
    fn next(&mut self) -> Option<u8>;

    #[inline]
    fn next_if<F>(&mut self, pred: F) -> Option<u8>
    where
        F: FnOnce(u8) -> bool,
    {
        if pred(self.peek()?) {
            self.next()
        } else {
            None
        }
    }

    #[inline]
    fn peek_char(&mut self) -> u8 {
        self.peek().unwrap_or(Self::EOF)
    }
    #[inline]
    fn next_char(&mut self) -> u8 {
        self.next().unwrap_or(Self::EOF)
    }
}

pub struct ReaderStream<R: Read> {
    pub(super) inner: Peekable<Bytes<BufReader<R>>>,
    pub(super) index: usize,
}
impl<R: Read> ReaderStream<R> {
    pub fn new(reader: R) -> ReaderStream<R> {
        Self {
            inner: BufReader::new(reader).bytes().peekable(),
            index: 0,
        }
    }
    pub fn from_file(file: impl AsRef<Path>) -> std::io::Result<ReaderStream<File>> {
        let inner = BufReader::new(File::open(file)?).bytes().peekable();
        Ok(ReaderStream::<File> { inner, index: 0 })
    }
}

impl<R: Read> Stream for ReaderStream<R> {
    fn peek(&mut self) -> Option<u8> {
        match self.inner.peek()? {
            Ok(ok) => Some(*ok),
            Err(_) => None,
        }
    }
    fn next(&mut self) -> Option<u8> {
        self.index += 1;
        match self.inner.next()? {
            Ok(ok) => Some(ok),
            Err(_) => None,
        }
    }

    fn index(&self) -> usize {
        self.index
    }
}

pub struct StrStream<'s> {
    pub(super) index: usize,
    pub(super) inner: Peekable<StrBytes<'s>>,
}

impl StrStream<'_> {
    pub fn new<'s>(string: &'s str) -> StrStream<'s> {
        StrStream::<'s> {
            index: 0,
            inner: string.bytes().peekable(),
        }
    }
}

impl<'s> Stream for StrStream<'s> {
    fn index(&self) -> usize {
        self.index
    }

    #[inline]
    fn peek(&mut self) -> Option<u8> {
        self.inner.peek().copied()
    }

    #[inline]
    fn next(&mut self) -> Option<u8> {
        self.index += 1;
        self.inner.next()
    }
}
