//! Utilities used in `rpmspec`.
use parking_lot::RwLock;
use smartstring::alias::String;
use std::{
    io::{BufReader, Read},
    path::Path,
    sync::Arc,
};

use crate::ParseResult;

#[macro_export]
macro_rules! opt {
    ($cond:expr => $out:expr) => {
        if $cond {
            Some($out)
        } else {
            None
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Brace {
    Curly,
    Square,
    Round,
}

impl Brace {
    pub fn open_ch(ch: char) -> Option<Self> {
        Some(match ch {
            '{' => Self::Curly,
            '[' => Self::Square,
            '(' => Self::Round,
            _ => return None,
        })
    }
    pub fn close_ch(ch: char) -> Option<Self> {
        Some(match ch {
            '}' => Self::Curly,
            ']' => Self::Square,
            ')' => Self::Round,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Consumer<R: Read + ?Sized> {
    pub s: Arc<RwLock<String>>,
    pub r: Option<Arc<RwLock<BufReader<Box<R>>>>>,
    pub file: Arc<Path>,
    pub pos: usize,
    pub end: usize,
}

impl<R: Read> Default for Consumer<R> {
    fn default() -> Self {
        Self { s: Arc::new(RwLock::new("".into())), r: None, file: Arc::from(Path::new("")), pos: 0, end: 0 }
    }
}

impl<R: Read + ?Sized> Consumer<R> {
    pub fn new(s: Arc<RwLock<String>>, r: Option<Arc<RwLock<BufReader<Box<R>>>>>, file: Arc<Path>) -> Self {
        Self { end: 0, s, r, pos: 0, file }
    }
    #[must_use]
    pub fn range(&mut self, r: std::ops::Range<usize>) -> Option<Self> {
        let cur = self.pos;
        while self.s.read().len() < r.end {
            self.next()?;
        }
        self.pos = cur;
        Some(Self { s: Arc::clone(&self.s), r: None, file: Arc::clone(&self.file), pos: r.start, end: r.end })
    }
    #[must_use]
    pub fn range_string(&mut self, r: std::ops::Range<usize>) -> Option<String> {
        let cur = self.pos;
        self.pos = r.start;
        let mut ret = String::new();
        while self.pos != r.end {
            let Some(ch) = self.next() else {
                // unexpected EOF
                return None;
            };
            ret.push(ch);
        }
        self.pos = cur;
        Some(ret)
    }
    pub fn must_range_str(&self, r: std::ops::Range<usize>) -> parking_lot::MappedRwLockReadGuard<str> {
        parking_lot::RwLockReadGuard::map(self.s.read(), |s| s.get(r.start..r.end).unwrap())
    }
    #[inline]
    pub fn back(&mut self, last: char) {
        self.pos -= last.len_utf8();
    }
    pub fn until(&mut self, f: impl Fn(char) -> bool) {
        while let Some(ch) = self.next() {
            if f(ch) {
                self.back(ch);
                break;
            }
        }
    }
    /// Read all characters until (and not including) the character that satisfy `f()`
    pub fn read_before(&mut self, s: &mut String, f: impl Fn(char) -> bool) {
        while let Some(ch) = self.next() {
            if f(ch) {
                self.back(ch);
                break;
            }
            s.push(ch);
        }
    }
    /// Just like [`read_before()`], but includes the last character
    pub fn read_until(&mut self, s: &mut String, f: impl Fn(char) -> bool) {
        for ch in self.by_ref() {
            s.push(ch);
            if f(ch) {
                break;
            }
        }
    }
    pub fn after(&mut self, f: impl Fn(char) -> bool) {
        for ch in self.by_ref() {
            if f(ch) {
                return;
            }
        }
    }
    pub fn peek(&mut self) -> Option<char> {
        let p = self.pos;
        let out = self.next();
        self.pos = p;
        out
    }
    pub fn is_eof(&mut self) -> bool {
        self.peek().is_none()
    }
    /// Skip through everything until the consumer reaches a reasonable spot:
    /// - assume it starts with no quotes
    /// - the line ends
    /// - it's not inside quotes anymore
    ///
    /// "skip until end of…thing?"
    pub fn skip_til_eot(&mut self) -> ParseResult<()> {
        let mut ps = vec![];
        let mut backslash = false;
        let mut line_start = true;
        'main: while let Some(ch) = self.next() {
            if ch == '\\' {
                backslash = !backslash;
                continue;
            }
            if ch == '\n' {
                line_start = true;
                if backslash {
                    backslash = false;
                    continue;
                }
                if ps.is_empty() {
                    break;
                }
                continue;
            }
            if ch.is_whitespace() {
                // make backslash unchanged
                continue;
            }
            if backslash {
                // ignore current char because it's escaped
                backslash = false;
                continue;
            }
            if line_start && ch == '#' {
                // skip comment
                // we'll say this assumption holds if there is only
                // whitespace before the hashtag for this line
                self.until(|ch| ch == '\n');
                continue;
            }
            if line_start && ch == '-' && self.peek() == Some('-') {
                self.next().unwrap();
                if Some(' ') == self.peek() {
                    // assume this is a lua comment…?
                    // we'll say this assumption holds if there is only
                    // whitespace before `-- ` for this line
                    // FIXME: should do the parsing in a better way? This is a horrible assumption!
                    self.until(|ch| ch == '\n');
                    continue;
                }
            }
            line_start = false;
            if let Some(brace) = Brace::open_ch(ch) {
                ps.push(brace);
                continue;
            }
            if ['"', '\''].contains(&ch) {
                let quote = ch;
                // loop until found matching quote
                while let Some(ch) = self.next() {
                    if ch == '\\' {
                        self.next();
                        continue;
                    }
                    if ch == quote {
                        continue 'main;
                    }
                }
                return Err(format!("EOF while in {quote}string{quote}").into());
            }
            if let Some(close) = Brace::close_ch(ch) {
                let Some(open) = ps.pop() else {
                    return Err(format!("Unexpected closing brace `{ch}`").into());
                };
                if open != close {
                    return Err(format!("Expected closing {open:?}, found `{ch}`").into());
                }
                continue;
            }
        }
        Ok(())
    }
    pub fn read_til_eot(&mut self) -> ParseResult<parking_lot::MappedRwLockReadGuard<str>> {
        let start = self.pos;
        self.skip_til_eot()?;
        Ok(self.must_range_str(start..self.pos))
    }
    pub fn skip_til_endbrace(&mut self, brace: Brace) -> ParseResult<()> {
        let mut ps = vec![];
        let mut backslash = false;
        'main: while let Some(ch) = self.next() {
            if ch == '\\' {
                backslash = !backslash;
                continue;
            }
            if backslash {
                // ignore current char because it's escaped
                backslash = false;
                continue;
            }
            if let Some(brace) = Brace::open_ch(ch) {
                ps.push(brace);
                continue;
            }
            if ['"', '\''].contains(&ch) {
                let quote = ch;
                // loop until found matching quote
                while let Some(ch) = self.next() {
                    if ch == '\\' {
                        self.next();
                        continue;
                    }
                    if ch == quote {
                        continue 'main;
                    }
                }
                return Err(format!("EOF while in {quote}string{quote}").into());
            }
            if let Some(close) = Brace::close_ch(ch) {
                let Some(open) = ps.pop() else {
                    if close == brace {
                        return Ok(());
                    }
                    return Err(format!("Unexpected closing brace `{ch}`").into());
                };
                if open != close {
                    return Err(format!("Expected closing {open:?}, found `{ch}`").into());
                }
                continue;
            }
        }
        Err("Unexpected EOF".into())
    }
    pub fn read_before_endbrace(&mut self, brace: Brace) -> ParseResult<String> {
        let start = self.pos;
        self.skip_til_endbrace(brace)?;
        Ok(self.range_string(start..self.pos - 1).unwrap())
    }
}

impl<R: ?Sized + Read> Iterator for Consumer<R> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.end != 0 && self.pos >= self.end {
            return None;
        }
        if let Some(c) = self.s.read().get(self.pos..).and_then(|substr| substr.chars().next()) {
            self.pos += c.len_utf8();
            return Some(c);
        }
        let mut s = self.s.write();
        // we reacquire a lock and check if we still can't access it
        if let Some(c) = s.get(self.pos..).and_then(|substr| substr.chars().next()) {
            self.pos += c.len_utf8();
            return Some(c);
        }
        let mut buf = [0; 1024];
        // we can lock self.r here directly since self.s is definitely controlled
        let nbyte = self.r.as_mut()?.write().read(&mut buf).ok()?;
        if nbyte == 0 {
            return None; // EOF
        }
        s.push_str(core::str::from_utf8(&buf[..nbyte]).map_err(|e| format!("cannot parse buffer `{buf:?}`: {e}")).ok()?);
        let Some(c) = s.get(self.pos..).and_then(|substr| substr.chars().next()) else { panic!("Consumer has no `s[{}]` after reading from `r`, where `s` is: {s}", self.pos) };
        drop(s);
        self.pos += c.len_utf8();
        Some(c)
    }
}

impl<R: Read> From<&str> for Consumer<R> {
    fn from(s: &str) -> Self {
        Self::new(Arc::from(RwLock::new(s.into())), None, Arc::from(Path::new("<?>")))
    }
}

pub fn handle_line_skip(s: std::str::Chars) -> String {
    let mut out = String::new();
    let mut line_end_buf = String::new();
    for c in s {
        if c == '\n' {
            line_end_buf.clear();
            continue;
        }
        if c.is_whitespace() || c == '\\' {
            line_end_buf.push(c);
            continue;
        }
        if !line_end_buf.is_empty() {
            out.push_str(&line_end_buf);
            line_end_buf.clear();
        }
        out.push(c);
    }
    out
}
