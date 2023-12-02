//! In progress effort to implement a new cool REPL using termion and stuff
use rzcalc_core::Context;
use rzcalc_core::Eval;
use rzcalc_core::EvalError;
use rzcalc_core::Node;
use rzcalc_core::Parser;
use rzcalc_core::RzError;
use rzcalc_core::Value;
use std::io::{self, Write};
use termion::clear;
use termion::cursor;

use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;

use crate::highlight::{HighlightedNode, HighlightedValue};

pub struct Repl {
    buffer: Vec<char>,
    buffer_cursor: usize,
    popup: Vec<String>,
    popup_cursor: usize,
}

impl Repl {
    pub fn new() -> Self {
        Self {
            buffer: Vec::with_capacity(1024),
            buffer_cursor: 0,
            popup: Vec::with_capacity(128),
            popup_cursor: 0,
        }
    }
    pub fn clear(&mut self) {
        self.buffer.clear();
        self.buffer_cursor = 0;
        self.popup.clear();
        self.popup_cursor = 0;
    }

    pub fn take(&mut self) -> String {
        let result = self.buffer.iter().collect();
        self.clear();
        result
    }

    pub fn insert_char(&mut self, x: char) {
        self.buffer.insert(self.buffer_cursor, x);
        self.buffer_cursor += 1;
    }

    pub fn backspace(&mut self) {
        if self.buffer_cursor > 0 {
            self.buffer.remove(self.buffer_cursor - 1);
            self.buffer_cursor -= 1;
        }
    }

    pub fn home(&mut self) {
        self.buffer_cursor = 0;
    }

    pub fn end(&mut self) {
        self.buffer_cursor = self.buffer.len();
    }

    pub fn up(&mut self) {
        if self.popup_cursor > 0 {
            self.popup_cursor = self.popup_cursor.wrapping_sub(1);
        }
    }

    pub fn down(&mut self) {
        if self.popup_cursor < self.popup.len().wrapping_sub(1) {
            self.popup_cursor += 1
        }
    }

    pub fn left_word(&mut self) {
        while self.buffer_cursor > 0
            && self.buffer_cursor <= self.buffer.len()
            && !self
                .buffer
                .get(self.buffer_cursor - 1)
                .unwrap()
                .is_alphanumeric()
        {
            self.buffer_cursor -= 1;
        }
        while self.buffer_cursor > 0
            && self.buffer_cursor <= self.buffer.len()
            && self
                .buffer
                .get(self.buffer_cursor - 1)
                .unwrap()
                .is_alphanumeric()
        {
            self.buffer_cursor -= 1;
        }
    }

    pub fn right_word(&mut self) {
        while self.buffer_cursor < self.buffer.len()
            && !self
                .buffer
                .get(self.buffer_cursor)
                .unwrap()
                .is_alphanumeric()
        {
            self.buffer_cursor += 1;
        }
        while self.buffer_cursor < self.buffer.len()
            && self
                .buffer
                .get(self.buffer_cursor)
                .unwrap()
                .is_alphanumeric()
        {
            self.buffer_cursor += 1;
        }
    }

    pub fn left_char(&mut self) {
        if self.buffer_cursor > 0 {
            self.buffer_cursor -= 1;
        }
    }

    pub fn right_char(&mut self) {
        if self.buffer_cursor < self.buffer.len() {
            self.buffer_cursor += 1;
        }
    }

    pub fn render(&self, prompt: &str, sink: &mut impl Write) -> io::Result<()> {
        const POPUP_SIZE: usize = 5;
        let buffer: String = self.buffer.iter().collect();
        write!(sink, "\r{}{}{}\r\n", clear::AfterCursor, prompt, &buffer)?;
        for (index, line) in self.popup.iter().take(POPUP_SIZE).enumerate() {
            if index == self.popup_cursor {
                write!(sink, ">")?
            } else {
                write!(sink, " ")?
            }
            write!(sink, " {}\r\n", line)?;
        }
        write!(
            sink,
            "{}{}",
            cursor::Up((POPUP_SIZE.min(self.popup.len()) + 1) as _),
            cursor::Right((prompt.len() + self.buffer_cursor) as _)
        )?;
        Ok(())
    }
}

impl Default for Repl {
    fn default() -> Self {
        Self::new()
    }
}

impl Repl {
    pub fn start(&mut self, ctx: &mut Context<'_>) {
        // TODO: check if the stdin is tty
        // If it is not maybe switch to the old/simplified REPL
        let prompt = "rscalc > ";
        let mut stdout = io::stdout()
            .into_raw_mode()
            .expect("Failed to convert stdout into raw mode");
        let stdin = io::stdin();
        write!(stdout, "{}", prompt).unwrap();
        stdout.flush().unwrap();

        'loops: for key in stdin.keys() {
            match key.unwrap() {
                Key::Char('\n') => {
                    write!(stdout, "\r\n").ok();
                    let take = self.take();
                    if &take == "quit" {
                        break 'loops;
                    }
                    match Parser::parse_string(take).parse() {
                        Ok(expr) => match expr.eval(ctx) {
                            Ok(value) => print_value(&expr, value),
                            Err(err) => print_eval_error(&expr, err),
                        },
                        Err(err) => print_error(err),
                    }
                }
                Key::Ctrl('a') | Key::Home => self.home(),
                Key::Ctrl('e') | Key::End => self.end(),
                Key::Ctrl('b') | Key::Left => self.left_char(),
                Key::Ctrl('f') | Key::Right => self.right_char(),
                Key::Ctrl('n') | Key::Down => self.down(),
                Key::Ctrl('p') | Key::Up => self.up(),
                Key::Ctrl('c') => {
                    write!(stdout, "^C\r\n").ok();
                    break 'loops;
                }
                Key::Alt('b') => self.left_word(),
                Key::Alt('f') => self.right_word(),
                Key::Char(key) => {
                    self.insert_char(key);
                    self.popup.clear();
                }
                Key::Backspace => self.backspace(),
                _ => {}
            }
            self.render(prompt, &mut stdout).unwrap();
            stdout.flush().unwrap();
        }
    }
}
pub fn print_error(err: RzError) {
    match err {
        RzError::ParseError(err) => eprintln!("{}{err}", clear::UntilNewline),
        RzError::EvalError(err) => eprintln!("ERROR: {err}"),
        RzError::IoError(err) => eprintln!("ERROR: {err}"),
        RzError::Any(any) => eprintln!("ERROR: {any}"),
    }
    eprintln!();
}
pub fn print_eval_error(expr: &Node, err: EvalError) {
    eprintln!();
    eprintln!(
        "ERROR: on evaluate expr\r\n    => {}\r\n{err}",
        HighlightedNode(expr)
    );
    eprintln!();
}

pub fn print_value(expr: &Node, value: Value) {
    eprintln!();
    eprintln!(
        "= {}\r\n= {}",
        HighlightedNode(expr),
        HighlightedValue(&value)
    );
    eprintln!();
}
