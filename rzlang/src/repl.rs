//! In progress effort to implement a new cool REPL using termion and stuff
use rzcalc_core::{Context, Eval, Node, Parser, RzError, Value};
use std::io::{self, Stdout, Write};

use termion::{
    clear, cursor,
    event::{Event, Key},
    input::TermRead,
    raw::{IntoRawMode, RawTerminal},
};

use crate::highlight::{HighlightedNode, HighlightedValue};

pub struct Repl {
    quit: bool,
    buffer: Vec<char>,
    buffer_cursor: usize,
    popup: Vec<String>,
    popup_cursor: usize,
}

impl Repl {
    pub fn new() -> Self {
        Self {
            quit: false,
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
    fn on_key(&mut self, stdout: &mut RawTerminal<Stdout>, ctx: &mut Context<'_>, key: Key) {
        match key {
            Key::Char('\n') => {
                write!(stdout, "\r\n").ok();
                let take = self.take();
                if &take == "quit" {
                    self.quit = true;
                    return;
                }
                match Parser::parse_string(&take).and_then(|x| x.eval(ctx).map(|v| (x, v))) {
                    Ok((expr, value)) => print_value(&expr, value),
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
                self.quit = true;
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
    }
    pub fn start(&mut self, ctx: &mut Context<'_>) {
        // TODO: check if the stdin is tty
        // If it is not maybe switch to the old/simplified REPL
        let prompt = "rscalc > ";
        let mut stdout = io::stdout()
            .into_raw_mode()
            .expect("Failed to convert stdout into raw mode");
        write!(stdout, "{}", prompt).unwrap();
        stdout.flush().unwrap();

        for event in io::stdin().events() {
            match event {
                Ok(Event::Key(key)) => self.on_key(&mut stdout, ctx, key),
                Ok(Event::Mouse(_mouse)) => {}
                Ok(_) => {}
                Err(err) => print_error(From::from(err)),
            }
            self.render(prompt, &mut stdout).unwrap();
            stdout.flush().unwrap();

            if self.quit {
                break;
            }
        }
    }
}
pub fn print_error(err: RzError) {
    eprintln!();
    eprintln!("{err}");
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
