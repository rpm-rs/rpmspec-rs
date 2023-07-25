use crate::{error::ParserError, parse::SpecParser, util::Consumer};
use parking_lot::Mutex;
use smartstring::alias::String;
use std::{collections::HashMap, io::Read, path::Path, sync::Arc};
use color_eyre::eyre::eyre;

#[derive(Clone)]
pub enum MacroType {
	Internal(fn(&mut SpecParser, &mut String, &mut Consumer<dyn Read + '_>) -> Result<(), ParserError>),
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
			fn $m($p: &mut SpecParser, $o: &mut String, $r: &mut Consumer<dyn Read + '_>) -> Result<(), ParserError> $body
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
		let (name, param): (String, bool) = name.strip_suffix("()").map_or_else(
			|| (name.into(), false),
			|x| (x.into(), true),
		);
		let csm = r.range(pos+1+name.len()..r.pos).ok_or_else(|| eyre!("%define: cannot unwind Consumer"))?;
		p.define_macro(name, csm, param, def.len());
		Ok(())
	}
	macro undefine(p, _o, r) {
		p.macros.remove(&r.read_til_eol().unwrap());
		Ok(())
	}
);
