use smartstring::alias::String;
use std::{ops::Index, path::Path, sync::Arc, io::Read};
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
pub struct Consumer<R: std::io::Read = std::fs::File> {
	pub s: Arc<str>,
	pub r: Option<Arc<std::io::BufReader<R>>>,
	pub file: Arc<Path>,
	pub pos: usize,
}

impl<R: Read> Consumer<R> {
	pub fn new(s: Arc<str>, r: Option<Arc<std::io::BufReader<R>>>, file: Arc<Path>) -> Self {
		Self { s, r, pos: 0, file }
	}
	pub fn range(&mut self, r: std::ops::Range<usize>) -> Option<Consumer<R>> {
		let cur = self.pos;
		while self.pos >= r.end {
			self.next()?;
		}
		self.pos = cur;
		Some(Self { s: Arc::from(self.s.index(r)), r: None, file: self.file, pos: r.start })
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

impl<R: Read> Iterator for Consumer<R> {
	type Item = char;

	fn next(&mut self) -> Option<Self::Item> {
		if let Some(c) = self.s.chars().nth(self.pos) {
			self.pos += 1;
			return Some(c);
		}
		let mut buf: Vec<u8>;
		if self.r?.read(&mut buf).ok()? == 0 {
			return None; // EOF
		}
		self.s = Arc::from(core::str::from_utf8(&buf).map_err(|e| error!("cannot parse buffer `{buf:?}`: {e}")).ok()?);
		let c = unsafe { self.s.chars().nth(self.pos).unwrap_unchecked() };
		self.pos += 1;
		return Some(c);
	}
}

// somehow you need this to export the macro
pub(crate) use gen_read_helper;

pub mod textproc {
	use color_eyre::{eyre::eyre, Result};
	use smartstring::alias::String;
	use tracing::{debug, warn};

	use crate::parse::SpecParser;

	use super::Consumer;

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

	pub fn back(reader: &mut Consumer, quotes: &mut String, ch: char) -> Result<()> {
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

	/// Expand macros depending on `notflag`.
	///
	/// when %a is undefined, %{!a} expands to %{!a}, but %!a expands to %a.
	pub fn macro_expand_notflagproc(parser: &mut SpecParser, notflag: bool, reader: &mut Consumer, content: &str, name: &str, out: &mut String) {
		let res = parser._rp_macro(name, reader);
		if notflag {
			if let Ok(s) = res {
				out.push_str(&s);
			}
			return;
		}
		out.push_str(&res.unwrap_or_else(|e| {
			debug!("_rp_macro: {e:#}");
			if content.is_empty() { format!("%{name}") } else { format!("%{{!{name}}}") }.into()
		}));
	}
}
