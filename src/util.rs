use parking_lot::Mutex;
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
macro_rules! gen_read_helper {
	($reader:ident $quotes:ident) => {
		#[allow(unused_macros)]
		macro_rules! exit_chk {
			() => {
				if !$quotes.is_empty() {
					return Err(eyre!("Unclosed quotes: `{}`", $quotes));
				}
			};
		}
		#[allow(unused_macros)]
		macro_rules! next {
			(#) => {{
				let Some(ch) = $reader.next() else { exit!(); };
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
	};
}

#[derive(Debug, Clone)]
pub struct Consumer<R: Read + ?Sized = stringreader::StringReader<'static>> {
	pub s: Arc<Mutex<String>>,
	pub r: Option<Arc<Mutex<BufReader<Box<R>>>>>,
	pub file: Arc<Path>,
	pub pos: usize,
	pub end: usize,
}

impl Default for Consumer {
	fn default() -> Self {
		Self { s: Arc::new(Mutex::new("".into())), r: None, file: Arc::from(Path::new("")), pos: 0, end: 0 }
	}
}

impl<R: Read + ?Sized> Consumer<R> {
	pub fn new(s: Arc<Mutex<String>>, r: Option<Arc<Mutex<BufReader<Box<R>>>>>, file: Arc<Path>) -> Self {
		Self { end: 0, s, r, pos: 0, file }
	}
	#[must_use]
	pub fn range(&mut self, r: std::ops::Range<usize>) -> Option<Consumer<R>> {
		let cur = self.pos;
		while self.s.lock().len() < r.end {
			self.next()?;
		}
		self.pos = cur;
		Some(Self { s: Arc::clone(&self.s), r: None, file: Arc::clone(&self.file), pos: r.start, end: r.end })
	}
	#[inline]
	pub fn back(&mut self) {
		self.pos -= 1;
	}
	pub fn read_til_eol(&mut self) -> Option<String> {
		let mut ps = vec![];
		let mut out = String::new();
		macro_rules! close {
			($ch:ident ~ $begin:expr, $end:expr) => {
				if $ch == $end {
					match ps.pop() {
						Some($begin) => continue,
						Some(x) => {
							error!("Found `{}` before closing `{x}`", $end);
							return None;
						}
						None => {
							error!("Unexpected closing char: `{}`", $end);
							return None;
						}
					}
				}
			};
		}
		'main: while let Some(ch) = self.next() {
			if ch == '\0' {
				// idk how it happens
				break;
			}
			if ch == '\n' {
				break;
			}
			if "([{".contains(ch) {
				ps.push(ch);
				continue;
			}
			if ch == '\'' {
				ps.push('\'');
				for ch in self.by_ref() {
					ps.push(ch);
					if ch == '\'' {
						continue 'main;
					}
				}
				error!("Unexpected EOF, `'` not closed");
				return None;
			}
			if ch == '"' {
				ps.push('"');
				for ch in self.by_ref() {
					ps.push(ch);
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
		let mut s = self.s.lock();
		if let Some(c) = s.chars().nth(self.pos) {
			self.pos += 1;
			return Some(c);
		}
		let mut buf = [0; 64];
		let nbyte = self.r.as_mut()?.lock().read(&mut buf).ok()?;
		if nbyte == 0 {
			return None; // EOF
		}
		s.push_str(core::str::from_utf8(&buf[..nbyte]).map_err(|e| color_eyre::eyre::eyre!("cannot parse buffer `{buf:?}`: {e}")).ok()?);
		let c = unsafe { s.chars().nth(self.pos).unwrap_unchecked() };
		self.pos += 1;
		return Some(c);
	}
}

impl From<&str> for Consumer {
	fn from(s: &str) -> Self {
		Consumer::new(Arc::from(Mutex::new(s.into())), None, Arc::from(Path::new("<?>")))
	}
}

// somehow you need this to export the macro
pub(crate) use gen_read_helper;

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
		} else if ch == '"' {
			if quotes.ends_with('"') {
				quotes.pop();
			} else {
				quotes.push('"');
			}
		} else if "([{".contains(ch) {
			quotes.push(ch);
		} else if ")]}".contains(ch) {
			match quotes.pop().ok_or_else(|| eyre!("Found `{ch}` but there are no quotes to close"))? {
				'(' if ch != ')' => return Err(eyre!("Expected `)`, Found `{ch}` before closing `(`")),
				'[' if ch != ']' => return Err(eyre!("Expected `]`, Found `{ch}` before closing `['")),
				'{' if ch != '}' => return Err(eyre!("Expected `}}`, Found `{ch}` before closing `{{`")),
				_ => {}
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
			_ => {}
		}
		reader.back();
		Ok(())
	}

	/// Check for `?` and `!` in macro invocation, returns true if processed.
	pub fn flag(question: &mut bool, notflag: &mut bool, ch: char) -> bool {
		if ch == '!' {
			*notflag = !*notflag;
			return true;
		}
		if ch == '?' {
			if *question {
				warn!("Seeing double `?` flag in macro use. Ignoring.");
			}
			*question = true;
			return true;
		}
		false
	}
}
