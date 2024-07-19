//! Macros in RPM
//!
//! <https://rpm-software-management.github.io/rpm/manual/macros.html>
use crate::{parse::SpecParser, util::Consumer};
use itertools::Itertools;
use parking_lot::RwLock;
use rpmspec_common::util::handle_line_skip;
use rpmspec_common::{syntaxerr, PErr as PE};
use smartstring::alias::String;
use std::collections::HashMap;
use std::io::{Read, Write};
use std::{path::Path, sync::Arc};

type InternalMacroFn = fn(&mut SpecParser, &mut String, &mut Consumer<dyn Read + '_>) -> Result<(), PE>;

/// A macro used in a spec file
#[derive(Clone)]
pub enum MacroType {
    /// Represents a macro written in rust and defined internally
    Internal(InternalMacroFn),
    /// Represents a macro defined during runtime
    Runtime {
        /// Path to file with the macro defined
        file: Arc<Path>,
        /// Number of chars before the macro definition
        offset: usize,
        /// Length of macro definition
        len: usize,
        /// The entire file pretty much
        s: Arc<RwLock<String>>,
        /// Is this a parameterized macro?
        param: bool,
    },
}

impl std::fmt::Debug for MacroType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Internal(_) => f.write_str("<builtin>")?,
            Self::Runtime { offset, len, s, .. } => f.write_str(&s.read()[*offset..offset + len])?,
        }
        Ok(())
    }
}

impl From<&str> for MacroType {
    fn from(value: &str) -> Self {
        Self::Runtime { file: Arc::from(Path::new("unknown")), offset: 0, s: Arc::new(RwLock::new(value.into())), param: false, len: value.len() }
    }
}

/// Helper macro to define RPM macros declaratively
macro_rules! __internal_macros {
    ($(macro $m:ident($p:pat, $o:pat, $r:pat) $body:block )+) => {
        $(
            #[allow(non_snake_case, clippy::unnecessary_wraps)]
            fn $m($p: &mut SpecParser, $o: &mut String, $r: &mut Consumer<dyn Read + '_>) -> Result<(), PE> $body
        )+
        lazy_static::lazy_static! {
            /// A list of macros defined in rust internally
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

// TODO: export a macro similar to `__internal_macros!` that allows users of this crate to
// define their own macros, possibly proc macro that one can do from module
//
//
// ```
// define_macros! {
//   macro my_macro(p, o, r) {
//      // do something
//
//   }
//
// }

// you will see some `#[rustfmt::skip]`, this is related to
// https://github.com/rust-lang/rustfmt/issues/5866
__internal_macros!(
    macro define(p, _, r) {
        r.until(|ch| !ch.is_whitespace());
        let pos = r.pos;
        let def = handle_line_skip(r.read_til_eot()?.chars());
        let def = def.trim_start();
        #[rustfmt::skip]
        let Some((name, _)) = def.split_once(' ') else {
            syntaxerr!(BadArgCount { expected: &[2], found: def.chars().filter(|&c| c == ' ').count() }@r.current_span());
        };
        let (name, param): (String, bool) = name.strip_suffix("()").map_or_else(|| (name.into(), false), |x| (x.into(), true));
        let csm = r.range(pos + 1 + name.len()..r.pos).expect("%define: cannot unwind Consumer");
        p.define_macro(name, &csm, param, csm.end - csm.pos - r.peek().map_or(0, |_| 1)); // remove \n only if not EOF
        Ok(())
    }
    macro global(p, o, r) {
        define(p, o, r)
    }
    macro undefine(p, _, r) {
        p.macros.remove(&*r.read_til_eot()?);
        Ok(())
    }
    macro load(p, _, r) {
        let f: String = r.collect();
        p.load_macro_from_file(std::path::Path::new(&*f))?;
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

        let new_reader = r.range(r.pos..r.end).expect("%expand: cannot unwind Consumer");

        // SAFETY:
        // This is a valid downcast because `new_reader.r` is `None` given
        // that it is created from `Consumer::range()`. Therefore, changing
        // `<R>` to anything should not affect the actual reader.
        let mut new_reader = *unsafe { Box::from_raw(Box::into_raw(Box::new(new_reader)).cast()) };
        p.parse_macro::<std::fs::File>(o, &mut new_reader)?;
        // r.pos = new_reader.pos;
        Ok(())
    }
    macro expr(p, o, r) {
        let mut rawexpression = String::new();
        expand(p, &mut rawexpression, r)?;
        // any type will do
        let mut csm: Consumer<std::fs::File> = Consumer::new(Arc::new(RwLock::new(rawexpression)), None, Arc::from(Path::new("<expr>")));
        p.parse_expr(o, &mut csm)?;
        Ok(())
    }
    macro lua(p, o, r) {
        let content: String = r.collect();
        let content = content.lines().map(|line| line.trim_end().trim_end_matches('\\')).join("\n");
        let parser = Arc::new(RwLock::new(std::mem::take(p)));
        let out = crate::lua::run(&parser, &content)?;
        std::mem::swap(p, &mut Arc::try_unwrap(parser).expect("cannot unwrap Arc rpmparser in %lua").into_inner());
        o.push_str(&out);
        Ok(())
    }
    macro macrobody(p, o, r) {
        let name = r.collect();
        #[rustfmt::skip]
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
            },
        }
        Ok(())
    }
    macro quote(_, o, r) {
        o.push('"');
        o.push_str(&r.collect::<String>());
        o.push('"');
        Ok(())
    }
    // macro gsub(p, o, r) {
    //     todo!()
    // }
    macro len(_, o, r) {
        o.push_str(&r.collect::<Box<[char]>>().len().to_string());
        Ok(())
    }
    macro lower(_, o, r) {
        // assume it's ascii?
        o.push_str(&r.collect::<String>().to_ascii_lowercase());
        Ok(())
    }
    // macro rep(p, o, r) {
    //     todo!()
    // }
    macro reverse(_, o, r) {
        let mut chs = r.collect::<Box<[char]>>();
        chs.reverse();
        chs.iter().for_each(|ch| o.push(*ch));
        Ok(())
    }
    // macro sub(p, o, r) {
    //     todo!()
    // }
    macro upper(_, o, r) {
        // assume it's ascii?
        o.push_str(&r.collect::<String>().to_ascii_uppercase());
        Ok(())
    }
    macro shescape(_, o, r) {
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
    macro shrink(_, o, r) {
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
    macro basename(_, o, r) {
        // according to testing this has nothing to do with the `basename` command
        let s: String = r.collect();
        o.push_str(s.rsplit_once('/').map_or(&s, |(_, x)| x));
        Ok(())
    }
    macro dirname(_, o, r) {
        let s: String = r.collect();
        o.push_str(s.rsplit_once('/').map_or(&s, |(x, _)| x));
        Ok(())
    }
    macro exists(_, o, r) {
        o.push(if Path::new(&*r.collect::<String>()).exists() { '1' } else { '0' });
        Ok(())
    }
    macro suffix(_, o, r) {
        let s: String = r.collect();
        o.push_str(s.rsplit_once('.').map_or("", |(_, x)| x));
        Ok(())
    }
    macro url2path(_, o, r) {
        // ? https://github.com/rpm-software-management/rpm/blob/master/rpmio/url.c#L50
        let s: String = r.collect();
        #[rustfmt::skip]
        let Ok(url) = url::Url::parse(&s) else {
            o.push_str(&s);
            return Ok(());
        };
        if matches!(url.scheme(), "https" | "http" | "hkp" | "file" | "ftp") {
            o.push_str(url.path());
        } else {
            o.push_str(&s);
        }
        Ok(())
    }
    macro u2p(p, o, r) {
        url2path(p, o, r)
    }
    macro uncompress(_, o, r) {
        //? https://github.com/rpm-software-management/rpm/blob/master/tools/rpmuncompress.c#L69
        let pathstr: String = r.collect();
        let path = std::path::PathBuf::from(&*pathstr);
        let Ok(fmt) = file_format::FileFormat::from_file(&path) else { return Ok(()) };
        o.push_str(match fmt {
            file_format::FileFormat::ArbitraryBinaryData => "cat ",
            file_format::FileFormat::Gzip => "gzip -dc ",
            file_format::FileFormat::Bzip2 => "bzip2 -dc ",
            file_format::FileFormat::Zip => "unzip ",
            file_format::FileFormat::LempelZivMarkovChainAlgorithm | file_format::FileFormat::Xz => "xz -dc ",
            file_format::FileFormat::Lzip => "lzip -dc ",
            file_format::FileFormat::LongRangeZip => "lrzip -dqo- ",
            file_format::FileFormat::SevenZip => "7zip x ",
            file_format::FileFormat::Zstandard => "zstd -dc ",
            file_format::FileFormat::TapeArchive if path.extension() == Some(std::ffi::OsStr::new("gem")) => "gem unpack ",
            _ => return Err(PE::InternalMacroEmittedError(format!("Unexpected file format: {fmt:?}").into())),
        });
        o.push_str(&pathstr);
        Ok(())
    }
    macro getncpus(_, o, _) {
        o.push_str(&num_cpus::get().to_string());
        Ok(())
    }
    macro getconfidir(_, o, _) {
        let res = std::env::var("RPM_CONFIGDIR");
        if let Err(std::env::VarError::NotUnicode(s)) = res {
            return Err(PE::InternalMacroEmittedError(format!("%{{getconfdir}} failed: While grabbing env var `RPM_CONFIGDIR`: Non-unicode OsString {s:?}").into()));
        }
        o.push_str(res.as_ref().map(|x| &**x).unwrap_or("/usr/lib/rpm"));
        Ok(())
    }
    macro getenv(_, o, r) {
        let name: String = r.collect();
        match std::env::var(&*name) {
            Ok(x) => o.push_str(&x),
            Err(std::env::VarError::NotPresent) => {},
            Err(std::env::VarError::NotUnicode(s)) => return Err(PE::InternalMacroEmittedError(format!("%{{getenv:{name}}} failed: Non-unicode OsString {s:?}").into())),
        }
        Ok(())
    }
    macro rpmversion(_, o, _) {
        o.push_str(env!("CARGO_PKG_VERSION"));
        Ok(())
    }
    macro echo(_, _, r) {
        tracing::info!("{}", r.collect::<String>());
        Ok(())
    }
    macro warn(_, _, r) {
        tracing::warn!("{}", r.collect::<String>());
        Ok(())
    }
    macro error(_, _, r) {
        tracing::error!("{}", r.collect::<String>());
        Ok(())
    }
    macro verbose(_, o, _) {
        // FIXME
        o.push('0');
        Ok(())
    }
    macro S(p, o, r) {
        // FIXME?
        expand(p, o, &mut Consumer::new(Arc::new(RwLock::new("%SOURCE".into())), None, r.file.clone()))?;
        r.for_each(|c| o.push(c));
        Ok(())
    }
    macro P(p, o, r) {
        // FIXME?
        expand(p, o, &mut Consumer::new(Arc::new(RwLock::new("%PATCH".into())), None, r.file.clone()))?;
        r.for_each(|c| o.push(c));
        Ok(())
    }
    macro trace(_, _, _) {
        tracing::warn!("`%trace` is not supposed by rpmspec-rs");
        Ok(())
    }
    macro dump(p, _, r) {
        let args = r.collect::<String>();
        if !args.is_empty() {
            tracing::warn!(?args, "Unexpected arguments to %dump");
        }
        let mut stdout = std::io::stdout().lock();
        for (k, v) in &p.macros {
            if let Some(v) = v.last() {
                let MacroType::Runtime { file, offset, len, s, param } = v else {
                    stdout.write_fmt(format_args!("[<internal>]\t%{k}\t<builtin>\n"))?;
                    continue;
                };
                //? https://github.com/rust-lang/rust-clippy/issues/11279
                let ss = s.read();
                let front = &ss[..*offset];
                let nline = front.chars().filter(|c| *c == '\n').count() + 1;
                let col = offset - front.find('\n').unwrap_or(0);
                let f = file.display();
                let p = if *param { "{}" } else { "" };
                let inner = &ss[*offset..*offset + *len];
                stdout.write_fmt(format_args!("[{f}:{nline}:{col}]\t%{k}{p}\t{inner}\n"))?;
            }
        }
        Ok(())
    }
);
