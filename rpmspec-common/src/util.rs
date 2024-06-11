//! Utilities used in `rpmspec`.
use color_eyre::eyre::eyre;
use parking_lot::RwLock;
use smartstring::alias::String;
use std::{
    io::{BufReader, Read},
    path::Path,
    sync::Arc,
};
use tracing::error;

/// string operations / parsing with consumer
///
/// # Requires
/// - `exit!()`
///
/// # Provides
/// - `exit_chk!()`
/// - `next!()`
#[rustfmt::skip] // kamo https://github.com/rust-lang/rustfmt/issues/4609
#[macro_export]
macro_rules! gen_read_helper {
    ($reader:ident $quotes:ident) => {
        #[allow(unused_macros)]
        macro_rules! exit_chk {
            () => {
                if !$quotes.is_empty() {
                    tracing::error!("");
                    return Err(eyre!("Unclosed quotes: `{}`", $quotes).into());
                }
            };
        }
        #[allow(unused_macros)]
        macro_rules! next {
            (#) => {{
                let Some(ch) = $reader.next() else {
                    exit!();
                };
                ch
            }};
            ($c:expr) => {
                if let Some(ch) = $reader.next() {
                    textproc::chk_ps(&mut $quotes, ch);
                    ch
                } else {
                    textproc::back($reader, $quotes, $c);
                    exit!();
                }
            };
            (~$c:expr) => {
                if let Some(ch) = $reader.next() {
                    ch
                } else {
                    $reader.back();
                    exit!();
                }
            };
        }
        #[allow(unused_macros)]
        macro_rules! exit_if_eof {
            () => {
                if $reader.is_eof() {
                    exit!();
                }
            };
            (else $method:ident) => {{
                let Some(x) = $reader.$method() else { exit!(); };
                x
            }};
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
    #[must_use]
    pub fn must_range_str(&self, r: std::ops::Range<usize>) -> parking_lot::MappedRwLockReadGuard<str> {
        parking_lot::RwLockReadGuard::map(self.s.read(), |s| s.get(r.start..r.end).unwrap())
    }
    #[inline]
    pub fn back(&mut self) {
        self.pos -= 1;
    }
    pub fn until(&mut self, f: impl Fn(char) -> bool) {
        while let Some(ch) = self.next() {
            if f(ch) {
                self.back();
                break;
            }
        }
    }
    /// Read all characters until (and not including) the character that satisfy `f()`
    pub fn read_before(&mut self, s: &mut String, f: impl Fn(char) -> bool) {
        while let Some(ch) = self.next() {
            if f(ch) {
                self.back();
                break;
            }
            s.push(ch);
        }
    }
    /// Just like [`read_before()`], but includes the last character
    pub fn read_until(&mut self, s: &mut String, f: impl Fn(char) -> bool) {
        while let Some(ch) = self.next() {
            s.push(ch);
            if f(ch) {
                break;
            }
        }
    }
    pub fn after(&mut self, f: impl Fn(char) -> bool) {
        while let Some(ch) = self.next() {
            if f(ch) {
                return;
            }
        }
    }
    pub fn peek(&mut self) -> Option<char> {
        let out = self.next();
        self.back();
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
    pub fn skip_til_eot(&mut self) -> color_eyre::Result<()> {
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
                return Err(eyre!("EOF while in {quote}string{quote}"));
            }
            if let Some(close) = Brace::close_ch(ch) {
                let Some(open) = ps.pop() else {
                    return Err(eyre!("Unexpected closing brace `{ch}`"));
                };
                if open != close {
                    return Err(eyre!("Expected closing {open:?}, found `{ch}`"));
                }
                continue;
            }
        }
        Ok(())
    }
    pub fn read_til_eot(&mut self) -> color_eyre::Result<parking_lot::MappedRwLockReadGuard<str>> {
        let start = self.pos;
        self.skip_til_eot()?;
        Ok(self.must_range_str(start..self.pos))
    }
    pub fn skip_til_endbrace(&mut self, brace: Brace) -> color_eyre::Result<()> {
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
                return Err(eyre!("EOF while in {quote}string{quote}"));
            }
            if let Some(close) = Brace::close_ch(ch) {
                let Some(open) = ps.pop() else {
                    if close == brace {
                        return Ok(());
                    }
                    return Err(eyre!("Unexpected closing brace `{ch}`"));
                };
                if open != close {
                    return Err(eyre!("Expected closing {open:?}, found `{ch}`"));
                }
                continue;
            }
        }
        Err(eyre!("Unexpected EOF"))
    }
    // TODO: Result<> instead
    pub fn read_til_eol(&mut self) -> Option<String> {
        let mut ps = String::new();
        let mut out = String::new();
        macro_rules! close {
            ($ch:ident ~ $begin:expr, $end:expr) => {
                if $ch == $end {
                    match ps.pop() {
                        Some($begin) => continue,
                        Some(x) => {
                            error!("Found `{}` before closing `{x}`", $end);
                            return None;
                        },
                        None => {
                            error!("Unexpected closing char: `{}`", $end);
                            return None;
                        },
                    }
                }
            };
        }
        'main: while let Some(ch) = self.next() {
            if ch == '\0' {
                // idk how it happens
                tracing::warn!("Found null byte");
                break;
            }
            if ch == '\n' {
                out.push('\n');
                if ps.is_empty() {
                    break;
                } else {
                    continue;
                }
            }
            if "([{".contains(ch) {
                ps.push(ch);
                continue;
            }
            if ch == '\'' {
                out.push('\'');
                for ch in self.by_ref() {
                    out.push(ch);
                    if ch == '\'' {
                        continue 'main;
                    }
                }
                error!("Unexpected EOF, `'` not closed");
                return None;
            }
            if ch == '"' {
                out.push('"');
                for ch in self.by_ref() {
                    out.push(ch);
                    if ch == '"' {
                        continue 'main;
                    }
                }
                error!("Unexpected EOF, `\"` not closed");
                return None;
            }
            close!(ch ~ '(', ')');
            close!(ch ~ '[', ']');
            close!(ch ~ '{', '}');
            out.push(ch);
        }
        if !ps.is_empty() {
            error!("Unclosed: {ps:?}");
            return None;
        }
        if out.is_empty() {
            return None;
        }
        Some(out)
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
        s.push_str(core::str::from_utf8(&buf[..nbyte]).map_err(|e| color_eyre::eyre::eyre!("cannot parse buffer `{buf:?}`: {e}")).ok()?);
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

pub mod textproc {
    use super::Consumer;
    use color_eyre::{eyre::eyre, Result};
    use smartstring::alias::String;
    use tracing::warn;

    pub fn chk_ps(quotes: &mut String, ch: char) -> Result<()> {
        if ch == '\'' {
            if quotes.ends_with('\'') {
                quotes.pop();
            } else {
                quotes.push('\'');
            }
            return Ok(());
        }
        if ch == '"' {
            if quotes.ends_with('"') {
                quotes.pop();
            } else {
                quotes.push('"');
            }
            return Ok(());
        }
        let x = quotes.pop();
        if let Some('\'' | '"') = x {
            quotes.push(x.unwrap());
            return Ok(());
        }
        if let Some(x) = x {
            quotes.push(x);
        }
        if "([{".contains(ch) {
            quotes.push(ch);
        } else if ")]}".contains(ch) {
            match quotes.pop().ok_or_else(|| eyre!("Found `{ch}` but there are no quotes to close"))? {
                '(' if ch != ')' => return Err(eyre!("Expected `)`, Found `{ch}` before closing `(`")),
                '[' if ch != ']' => return Err(eyre!("Expected `]`, Found `{ch}` before closing `['")),
                '{' if ch != '}' => return Err(eyre!("Expected `}}`, Found `{ch}` before closing `{{`")),
                _ => {},
            }
        }
        Ok(())
    }

    pub fn back<R: std::io::Read>(reader: &mut Consumer<R>, quotes: &mut String, ch: char) -> Result<()> {
        if ch == '\'' {
            if quotes.ends_with('\'') {
                quotes.pop();
            } else {
                quotes.push('\'');
            }
        }
        if ch == '"' {
            if quotes.ends_with('"') {
                quotes.pop();
            } else {
                quotes.push('"');
            }
        }
        match ch {
            ')' => quotes.push('('),
            ']' => quotes.push('['),
            '}' => quotes.push('{'),
            '(' if quotes.pop() != Some('(') => return Err(eyre!("BUG: pushing back `(` failed quotes check")),
            '[' if quotes.pop() != Some('[') => return Err(eyre!("BUG: pushing back `[` failed quotes check")),
            '{' if quotes.pop() != Some('{') => return Err(eyre!("BUG: pushing back `{{` failed quotes check")),
            _ => {},
        }
        reader.back();
        Ok(())
    }

    /// Check for `?` and `!` in macro invocation, returns true if processed.
    pub fn flag(question: &mut bool, notflag: &mut bool, first: &mut bool, ch: char) -> Option<bool> {
        if ch == '!' {
            if !*first {
                return None;
            }
            *notflag = !*notflag;
            return Some(true);
        }
        if ch == '?' {
            if !*first {
                return None;
            }
            if *question {
                warn!("Seeing double `?` flag in macro use. Ignoring.");
            }
            *question = true;
            return Some(true);
        }
        *first = false;
        Some(false)
    }
}
