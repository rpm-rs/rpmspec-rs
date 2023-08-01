//! Handlers for Lua \
//! FYI RPM spec has tight integration with Lua \
//!
//! BTW everything here is heavily rewritten to use `rlua`
//!
//! original C code creates a lib which is not really supported
//! in rlua so we kinda have to register a custom type instead
//! aka. `rlua::UserData` \
//! there are two things: `rpm` and `posix`. See more information:
//! <https://rpm-software-management.github.io/rpm/manual/lua.html>
// #![allow(clippy::unnecessary_wraps, clippy::needless_pass_by_value)]
use crate::parse::SpecParser;
use parking_lot::Mutex;
use rlua::{Lua, Result};
use std::sync::Arc;
mod repl;

macro_rules! __lua {
	(@type $t:ty | $default:ty) => { $t };
	(@type | $default:ty) => { $default };
	($(mod $ext:ident{$(fn $name:ident($p:pat$(=>$pt:ty)?, $ctx:pat$(=>$ct:ty)?, $arg:pat$(=>$at:ty)?)$(: $res:ty)? $body:block)+})+) => {
		$(
			mod $ext {
				#[allow(unused_imports)]
				use crate::{macros::MacroType, parse::SpecParser};
				#[allow(unused_imports)]
				use base64::{engine::general_purpose::STANDARD, Engine};
				#[allow(unused_imports)]
				use rlua::{Context, ExternalError, Result};
				use parking_lot::Mutex;
				use std::sync::Arc;
				$(
					#[allow(clippy::unnecessary_wraps)]
					pub fn $name(
						$p: __lua!(@type $($pt)? | &Arc<Mutex<SpecParser>>),
						$ctx: __lua!(@type $($ct)? | Context),
						$arg: __lua!(@type $($at)? | String)
					) -> Result<__lua!(@type $($res)? | ())> $body
				)+
			}
		)+
		pub(crate) fn run(rpmparser: &Arc<Mutex<SpecParser>>, script: &str) -> Result<String> {
			let lua = Lua::new();
			let anda_out = Arc::new(Mutex::new(String::new()));
			lua.context(|ctx| -> rlua::Result<()> {
				let globals = ctx.globals();
				$(
					let $ext = ctx.create_table()?;
					$({
						let p = Arc::clone(rpmparser);
						$ext.set(stringify!($name), ctx.create_function(move |ctx, arg| $ext::$name(&p, ctx, arg))?)?;
					})+
					globals.set(stringify!($ext), $ext)?;
				)+
				let anda_out = anda_out.clone();
				globals.set(
					"print",
					ctx.create_function(move |_, s: String| {
						anda_out.lock().push_str(&s);
						Ok(())
					})?,
				)?;
				ctx.load(script).exec()?;
				Ok(())
			})?;
			Ok(Arc::try_unwrap(anda_out).expect("Cannot unwrap Arc for print() output in lua").into_inner())
		}
	};
}

__lua!(
	mod rpm {
		fn b64decode(_, _, arg): String {
			String::from_utf8(STANDARD.decode(arg).map_err(ExternalError::to_lua_err)?).map_err(ExternalError::to_lua_err)
		}
		fn b64encode(_, _, arg): String {
			Ok(STANDARD.encode(arg))
		}
		fn define(p, _, arg) {
			if let Some((name, def)) = arg.split_once(' ') {
				let mut def: String = def.into();
				let name: String = name.strip_suffix("()").map_or_else(
					|| name.into(),
					|name| {
						def.push(' ');
						name.into()
					},
				);
				p.lock().macros.insert(name.into(), vec![(*def).into()]);
				Ok(())
			} else {
				Err("Invalid syntax: `%define {def}`".to_lua_err())
			}
		}
		fn execute(_, _, args=>Vec<String>): i32 {
			Ok(std::process::Command::new(&args[0]).args(&args[1..]).status().map_err(rlua::ExternalError::to_lua_err)?.code().unwrap_or(-1))
		}
		fn expand(p, _, arg): String {
			let mut out = smartstring::SmartString::new();
			p.lock().parse_macro(&mut out, &mut (&*arg).into()).map_err(rlua::ExternalError::to_lua_err)?;
			Ok(out.to_string())
		}
		// glob(_, _, arg=>(String, Option<String>))
		fn interactive(_, _, _=>()) {
			super::repl::repl(); // lazy
			// todo mimic
			Ok(())
		}
		fn isdefined(p, _, name): (bool, bool) {
			if let Some(def) = p.lock().macros.get(&*name) {
				if let Some(MacroType::Runtime { param, .. }) = def.last() {
					return Ok((true, *param));
				}
			}
			Ok((false, false))
		}
		fn load(p, _, arg) {
			p.lock().load_macro_from_file(&std::path::PathBuf::from(arg)).map_err(rlua::ExternalError::to_lua_err)
		}
		// todo rpm.fd
		// * BEGIN: File operations
		fn open(_, _, _=>(String, String)) {
			todo!()
		}
		fn close(_, _, _=>()) {
			todo!()
		}
		fn flush(_, _, _=>()) {
			todo!()
		}
		fn read(_, _, _=>Option<usize>) {
			todo!()
		}
		fn seek(_, _, _=>(String, usize)) {
			todo!()
		}
		fn write(_, _, _=>(String, Option<usize>)) {
			todo!()
		}
		fn reopen(_, _, _) {
			todo!()
		}
		// ... END: File operations

		fn redirect2null(_, _, _) {
			todo!()
		}
		fn undefine(p, _, name) {
			p.lock().macros.remove(&*name).ok_or_else(|| "error undefining macro".to_lua_err())?;
			Ok(())
		}
		fn vercmp(_, _, vers=>(String, String)): i8 {
			use crate::tools::expr::Version;
			use std::str::FromStr;
			let (v1, v2) = vers;
			let (v1, v2) = (Version::from_str(&v1).map_err(ExternalError::to_lua_err)?, Version::from_str(&v2).map_err(ExternalError::to_lua_err)?);
			Ok(v1.cmp(&v2) as i8)
		}
	}
	mod posix {
		fn access(_, _, _=>(String, Option<String>)): bool {
			todo!()
		}
		fn chdir(_, _, _) {
			todo!()
		}
		fn chmod(_, _, _=>(String, String, String)) {
			todo!()
		}
		fn chown(_, _, _=>(String, String, String)) {
			todo!()
		}
		fn ctermid(_, _, _=>()): String {
			todo!()
		}
		fn dir(_, _, _=>Option<String>): Vec<String> {
			todo!()
		}
		fn errno(_, _, _=>()): (String, isize) {
			todo!()
		}
		fn exec(_, _, _=>Vec<String>) {
			todo!()
		}
		fn files(_, _, _=>Option<String>): Vec<String> {
			todo!()
		}
		fn fork(_, _, _=>()): isize {
			todo!()
		}
		fn getcwd(_, _, _=>()): String {
			todo!()
		}
		fn getenv(_, _, _=>()): String {
			todo!()
		}
		// getgroup() return type???
		fn getlogin(_, _, _=>()): String {
			todo!()
		}
		fn getpasswd(_, _, _=>Vec<String>): String {
			todo!()
		}
		fn getprocessid(_, _, _): usize {
			todo!()
		}
		fn kill(_, _, _=>(usize, Option<usize>)) {
			todo!()
		}
		fn link(_, _, _=>(String, String)) {
			todo!()
		}
		fn mkdir(_, _, _) {
			todo!()
		}
		fn mkfifo(_, _, _) {
			todo!()
		}
		// pathconf() return type???
		fn putenv(_, _, _) {
			todo!()
		}
		fn readlink(_, _, _): String {
			todo!()
		}
		fn rmdir(_, _, _) {
			todo!()
		}
		fn setgid(_, _, _) {
			todo!()
		}
		fn setuid(_, _, _) {
			todo!()
		}
		fn sleep(_, _, _=>usize) {
			todo!()
		}
		// stat() return type???
		fn symlink(_, _, _=>(String, String)) {
			todo!()
		}
		// sysconf() return type???
		// times() return type???
		fn ttyname(_, _, _=>usize) {
			todo!()
		}
		fn umask(_, _, _=>Option<String>): String {
			todo!()
		}
		fn uname(_, _, _): String {
			todo!()
		}
		fn utime(_, _, _=>(String, Option<usize>, Option<usize>)) {
			todo!()
		}
		fn wait(_, _, _=>usize) {
			todo!()
		}
		fn setenv(_, _, _=>(String, String, bool)) {
			todo!()
		}
		fn unsetenv(_, _, _) {
			todo!()
		}
	}
);
