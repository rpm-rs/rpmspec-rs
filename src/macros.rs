use crate::{error::ParserError, parse::SpecParser, util::Consumer};
use smartstring::alias::String;
use std::{collections::HashMap, io::Read, path::Path, sync::Arc};

#[derive(Clone)]
pub enum MacroType {
	Internal(fn(&mut SpecParser, &mut String, &mut Consumer<dyn Read + '_>) -> Result<(), ParserError>),
	Runtime { file: Arc<Path>, offset: usize, def: Box<str>, param: bool },
}

impl std::fmt::Debug for MacroType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!("<MacroType>"))?;
		Ok(())
	}
}

impl From<&str> for MacroType {
	fn from(value: &str) -> Self {
		Self::Runtime { file: Arc::from(Path::new("unknown")), offset: 0, def: Box::from(value), param: false }
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
	macro undefine(p, o, r) {
		p.macros.remove(&r.read_til_eol().unwrap());
		Ok(())
	}
	macro hai(p, o, r) {
		p.macros.remove(&r.read_til_eol().unwrap());
		Ok(())
	}
);
