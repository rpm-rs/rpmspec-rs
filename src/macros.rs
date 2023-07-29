use crate::{error::ParserError as PE, parse::SpecParser, util::Consumer};
use color_eyre::eyre::eyre;
use parking_lot::Mutex;
use smartstring::alias::String;
use std::{collections::HashMap, io::Read, path::Path, sync::Arc};

#[derive(Clone)]
pub enum MacroType {
	Internal(fn(&mut SpecParser, &mut String, &mut Consumer<dyn Read + '_>) -> Result<(), PE>),
	Runtime { file: Arc<Path>, offset: usize, len: usize, s: Arc<Mutex<String>>, param: bool },
}

impl std::fmt::Debug for MacroType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!("<MacroType>"))?;
		Ok(())
	}
}

impl From<&str> for MacroType {
	fn from(value: &str) -> Self {
		Self::Runtime { file: Arc::from(Path::new("unknown")), offset: 0, s: Arc::new(Mutex::new(value.into())), param: false, len: value.len() }
	}
}

macro_rules! __internal_macros {
	($(macro $m:ident($p:ident, $o:ident, $r:ident) $body:block )+) => {
		$(
			fn $m($p: &mut SpecParser, $o: &mut String, $r: &mut Consumer<dyn Read + '_>) -> Result<(), PE> $body
		)+
		lazy_static::lazy_static! {
			pub static ref INTERNAL_MACROS: HashMap<String, Vec<MacroType>> = {
				let mut ret = HashMap::new();
				$({
					ret.insert(stringify!($m).into(), vec![MacroType::Internal($m)]);
				})+
				ret
			};
		}
	};
}

__internal_macros!(
	macro define(p, _o, r) {
		while let Some(ch) = r.next() {
			if !ch.is_whitespace() {
				r.back();
				break;
			}
		}
		let pos = r.pos;
		let def = r.read_til_eol().ok_or_else(|| eyre!("%define: read_til_eol() failed"))?;
		let def = def.trim_start();
		let Some((name, def)) = def.split_once(' ') else {
																																			return Err(eyre!("%define: Expected 2 arguments").into());
																																		};
		let def = def.trim();
		let (name, param): (String, bool) = name.strip_suffix("()").map_or_else(|| (name.into(), false), |x| (x.into(), true));
		let csm = r.range(pos + 1 + name.len()..r.pos).ok_or_else(|| eyre!("%define: cannot unwind Consumer"))?;
		p.define_macro(name, csm, param, def.len());
		Ok(())
	}
	macro global(p, o, r) {
		define(p, o, r)
	}
	macro undefine(p, _o, r) {
		p.macros.remove(&r.read_til_eol().unwrap());
		Ok(())
	}
	macro load(p, _o, r) {
		let f: String = r.collect();
		p.load_macro_from_file(&std::path::Path::new(&*f))?;
		Ok(())
	}
	macro expand(p, o, r) {
		// * Why downcasting `r` yet again?
		// Apparently `r` is `dyn` but we need to convert it to `impl` to use `p.parse_macro()`.
		// See `p._rp_macro()` for more info.
		// * Wait wait, won't `parse_macro()` call `_rp_macro()`?
		// Yeah... internal macros should not call `_rp_macro()` but we have no choice...
		// It will skip `Arc::try_unwrap()` inside `_rp_macro()` anyway since `new_reader.r` is
		// `None`. This should be safe.

		let new_reader = r.range(r.pos..r.end).ok_or_else(|| eyre!("Cannot wind Consumer in %expand"))?;

		// downcasting
		// note: by using `r.range()`, `new_reader.r` is `None`
		let mut new_reader = *unsafe { Box::from_raw(Box::into_raw(Box::new(new_reader)) as *mut Consumer<std::fs::File>) };
		p.parse_macro(o, &mut new_reader)?;
		// r.pos = new_reader.pos;
		Ok(())
	}
	macro expr(p, o, r) {
		todo!()
	}
	macro lua(p, o, r) {
		let content: String = r.collect();
		let parser = Arc::new(Mutex::new(std::mem::take(p)));
		let out = crate::lua::run(&parser, &content)?;
		std::mem::swap(p, &mut Arc::try_unwrap(parser).expect("Cannot unwrap Arc for print() output in lua").into_inner()); // break down Arc then break down Mutex
		o.push_str(&out);
		Ok(())
	}
	macro macrobody(p, o, r) {
		let name = r.collect();
		let Some(Some(m)) = p.macros.get(&name).map(|x| x.last()) else {
			return Err(PE::MacroNotFound(name));
		};
		match m {
			MacroType::Internal(_) => o.push_str("<builtin>"),
			MacroType::Runtime { file, offset, len, s, .. } => {
				// we can put anything as <R>
				let mut csm: Consumer<std::fs::File> = Consumer::new(Arc::clone(s), None, Arc::clone(file));
				csm.pos = *offset;
				csm.end = *offset + len;
				o.push_str(&csm.collect::<String>());
			}
		}
		Ok(())
	}
	macro quote(_p, o, r) {
		o.push('"');
		o.push_str(&r.collect::<String>());
		o.push('"');
		Ok(())
	}
	macro gsub(p, o, r) {
		todo!()
	}
	macro len(_p, o, r) {
		o.push_str(&r.collect::<Box<[char]>>().len().to_string());
		Ok(())
	}
	macro lower(_p, o, r) {
		// assume it's ascii?
		o.push_str(&r.collect::<String>().to_ascii_lowercase());
		Ok(())
	}
	macro rep(p, o, r) {
		todo!()
	}
	macro reverse(_p, o, r) {
		let mut chs = r.collect::<Box<[char]>>();
		chs.reverse();
		chs.into_iter().for_each(|ch| o.push(*ch));
		Ok(())
	}
	macro sub(p, o, r) {
		todo!()
	}
	macro upper(_p, o, r) {
		// assume it's ascii?
		o.push_str(&r.collect::<String>().to_ascii_uppercase());
		Ok(())
	}
	macro shescape(_p, o, r) {
		o.push('\'');
		for ch in r {
			if ch == '\'' {
				o.push('\'');
				o.push('\\');
				o.push('\'');
			}
			o.push(ch);
		}
		o.push('\'');
		Ok(())
	}
	macro shrink(_p, o, r) {
		while let Some(ch) = r.next() {
			if !ch.is_whitespace() {
				o.push(ch);
				break;
			}
		}
		let mut space = false;
		for ch in r {
			if ch.is_whitespace() {
				space = true;
				continue;
			}
			if space {
				space = false;
				o.push(' ');
			}
			o.push(ch);
		}
		Ok(())
	}
	macro basename(_p, o, r) {
		// according to testing this has nothing to do with the `basename` command
		let s: String = r.collect();
		o.push_str(s.rsplit_once('/').map_or(&s, |(_, x)| x));
		Ok(())
	}
	macro dirname(_p, o, r) {
		let s: String = r.collect();
		o.push_str(s.rsplit_once('/').map_or(&s, |(x, _)| x));
		Ok(())
	}
	macro exists(_p, o, r) {
		o.push(if Path::new(&*r.collect::<String>()).exists() { '1' } else { '0' });
		Ok(())
	}
	macro suffix(_p, o, r) {
		let s: String = r.collect();
		o.push_str(s.rsplit_once('.').map_or("", |(_, x)| x));
		Ok(())
	}
	macro url2path(p, o, r) {
		// ? https://github.com/rpm-software-management/rpm/blob/master/rpmio/url.c#L50
		let url: String = r.collect();
		let Ok(url) = url::Url::parse(&url) else {
			o.push('/');
			return Ok(());
		};
		if matches!(url.scheme(), "https" | "http" | "hkp" | "file" | "ftp") {
			o.push_str(url.path());
		}
		Ok(())
	}
	macro uncompress(p, o, r) {
		//? https://github.com/rpm-software-management/rpm/blob/master/tools/rpmuncompress.c#L69
		todo!()
	}
	macro getncpus(_p, o, r) {
		if r.next().is_some() {
			r.back();
			tracing::warn!(args=?r.collect::<String>(), "Unnecessary arguments supplied to `%getncpus`.");
		}
		o.push_str(&num_cpus::get().to_string());
		Ok(())
	}
	macro getconfidir(p, o, r) {
		todo!()
	}
	macro getenv(p, o, r) {
		todo!()
	}
	macro rpmversion(p, o, r) {
		todo!()
	}
	macro echo(p, o, r) {
		todo!()
	}
	macro warn(p, o, r) {
		todo!()
	}
	macro error(p, o, r) {
		todo!()
	}
	macro verbose(p, o, r) {
		todo!()
	}
	macro S(p, o, r) {
		todo!()
	}
	macro P(p, o, r) {
		todo!()
	}
	macro trace(p, o, r) {
		todo!()
	}
	macro dump(p, o, r) {
		todo!()
	}
);
