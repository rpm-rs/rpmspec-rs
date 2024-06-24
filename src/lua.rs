//! Handlers for Lua \
//! FYI RPM spec has tight integration with Lua \
//!
//! BTW everything here is heavily rewritten to use `mlua`
//!
//! original C code creates a lib which is not really supported
//! in mlua so we kinda have to register a custom type instead
//! aka. `mlua::UserData` \
//! there are two things: `rpm` and `posix`. See more information:
//! <https://rpm-software-management.github.io/rpm/manual/lua.html>
use crate::{macros::MacroType, parse::SpecParser};
use base64::{engine::general_purpose::STANDARD, Engine};
use core::ffi::CStr;
use mlua::{ExternalError, ExternalResult, Lua, Result, Value};
use parking_lot::RwLock;
use std::{
    ffi::CString,
    io::{Read, Seek, Write},
    str::FromStr,
    sync::Arc,
};

#[derive(Default)]
enum RpmFileIOType {
    Bzdio(bzip2::Decompress),
    Fdio,
    Gzdio(flate2::Decompress),
    #[default]
    Ufdio,
    Xzdio,
    Zstdio,
}
impl RpmFileIOType {
    fn process(&mut self, bs: Vec<u8>) -> mlua::Result<Vec<u8>> {
        match self {
            Self::Bzdio(dc) => {
                let mut buf = vec![];
                while dc.decompress_vec(&bs, &mut buf).into_lua_err()? == bzip2::Status::MemNeeded {
                    buf.try_reserve(buf.capacity()).into_lua_err()?;
                }
                Ok(buf)
            },
            Self::Fdio | Self::Ufdio => Ok(bs),
            Self::Gzdio(dc) => {
                let mut buf = vec![];
                dc.decompress_vec(&bs, &mut buf, flate2::FlushDecompress::None).into_lua_err()?;
                Ok(buf)
            },
            Self::Xzdio => {
                // let mut buf = vec![];
                // dc.read_to_end(&mut buf);
                // Ok(buf)

                // TODO: support for xzdio
                Err("rpmspec-rs does not yet support xzdio").into_lua_err()
            },
            // TODO: support for zsstdio
            Self::Zstdio => Err("rpmspec-rs does not yet support zstdio").into_lua_err(),
        }
    }
}

struct RpmFileMode {
    append: bool,
    write: bool,
    read: bool,
    fail_if_exist: bool,
    thread: bool,
    iodebug: bool,
    iotype: RpmFileIOType,
}

impl std::str::FromStr for RpmFileMode {
    type Err = mlua::Error;

    fn from_str(s: &str) -> Result<Self> {
        let (iotype, left) = if let Some((left, right)) = s.split_once('.') {
            let iotype = match right {
                "bzdio" => RpmFileIOType::Bzdio(bzip2::Decompress::new(false)),
                "fdio" => RpmFileIOType::Fdio,
                "gzdio" => RpmFileIOType::Gzdio(flate2::Decompress::new(true)),
                "ufdio" => RpmFileIOType::Ufdio,
                "xzdio" => RpmFileIOType::Xzdio,
                "zstdio" => RpmFileIOType::Zstdio,
                _ => return Err("Invalid iotype for rpm:open()/reopen()").into_lua_err(),
            };
            (iotype, left)
        } else {
            (RpmFileIOType::default(), s)
        };
        let append = left.contains('a');
        let write = left.contains('w') || left.contains('+');
        let read = left.contains('r') || left.contains('+');
        let fail_if_exist = left.contains('x');
        let thread = left.contains('T');
        let iodebug = left.contains('?');
        Ok(Self { append, write, read, fail_if_exist, thread, iodebug, iotype })
    }
}

struct RpmFile {
    path: String,
    innerfile: std::fs::File,
    modes: RpmFileMode,
}

impl RpmFile {
    fn new(path: String, mode: &str) -> mlua::Result<Self> {
        let modes = RpmFileMode::from_str(mode).into_lua_err()?;
        let mut f = std::fs::File::options();
        f.read(modes.read).write(modes.write);
        if modes.fail_if_exist {
            f.create_new(true);
        } else {
            f.create(true);
        }
        let mut innerfile = f.open(&path).into_lua_err()?;
        if modes.append {
            innerfile.seek(std::io::SeekFrom::End(0)).into_lua_err()?;
        }
        Ok(Self { path, innerfile, modes })
    }
}

impl mlua::UserData for RpmFile {
    fn add_methods<'lua, T: mlua::prelude::LuaUserDataMethods<'lua, Self>>(methods: &mut T) {
        methods.add_method("close", |_, _, ()| Ok(())); // literally do nothing
        methods.add_method_mut("flush", |_, this, ()| this.innerfile.flush().into_lua_err());
        methods.add_method_mut("read", |_, this, len: Option<usize>| {
            let Some(len) = len else {
                let mut buf = vec![];
                let size = this.innerfile.read_to_end(&mut buf).into_lua_err()?;
                buf.truncate(size);
                return this.modes.iotype.process(buf);
            };
            let mut buf = vec![0; len];
            this.innerfile.read_exact(&mut buf).into_lua_err()?;
            this.modes.iotype.process(buf)
        });
        methods.add_method_mut("seek", |_, this, (mode, offset): (String, isize)| {
            this.innerfile
                .seek(match &*mode {
                    "set" => std::io::SeekFrom::Start(offset.try_into().into_lua_err()?),
                    "cur" => std::io::SeekFrom::Current(offset.try_into().into_lua_err()?),
                    "end" => std::io::SeekFrom::End(offset.try_into().into_lua_err()?),
                    _ => return Err(format!("Invalid mode for seek(): `{mode}`")).into_lua_err()?,
                })
                .into_lua_err()
        });
        methods.add_method_mut("write", |_, this, (buf, len): (String, usize)| this.innerfile.write(buf[..len].as_bytes()).into_lua_err());
        methods.add_method_mut("reopen", |_, this, mode: String| {
            let f = Self::new(this.path.clone(), &mode).into_lua_err()?;
            drop(std::mem::replace(this, f));
            Ok(())
        });
    }
}

// https://github.com/amethyst/mlua/blob/master/examples/repl.rs
fn repl() {
    let lua = Lua::new();
    let mut editor = rustyline::DefaultEditor::new().expect("Failed to create editor");

    loop {
        let mut prompt = "> ";
        let mut line = String::new();

        loop {
            match editor.readline(prompt) {
                Ok(input) => line.push_str(&input),
                Err(_) => return,
            }

            match lua.load(&line).eval::<mlua::MultiValue>() {
                Ok(values) => {
                    editor.add_history_entry(line).unwrap();
                    println!("{}", values.iter().map(|value| format!("{value:#?}")).collect::<Vec<_>>().join("\t"));
                    break;
                },
                Err(mlua::Error::SyntaxError { incomplete_input: true, .. }) => {
                    // continue reading input and append it to `line`
                    line.push('\n'); // separate input lines
                    prompt = ">> ";
                },
                Err(e) => {
                    eprintln!("error: {e}");
                    break;
                },
            }
        }
    }
}

macro_rules! __lua {
    (@type $t:ty | $default:ty) => { $t };
    (@type | $default:ty) => { $default };
    ($(mod $ext:ident{$(fn $name:ident$(<$lua:lifetime>)?($p:pat, $ctx:pat, $arg:pat$(=>$at:ty)?)$(: $res:ty)? $body:block)+})+) => {
        $(
            mod $ext {
                use super::*;
                $(
                    #[allow(clippy::unnecessary_wraps)]
                    pub fn $name$(<$lua>)?(
                        $p: &Arc<RwLock<SpecParser>>,
                        $ctx: &$($lua)?mlua::Lua,
                        $arg: __lua!(@type $($at)? | String)
                    ) -> Result<__lua!(@type $($res)? | ())> $body
                )+
            }
        )+
        pub(crate) fn run(rpmparser: &Arc<RwLock<SpecParser>>, script: &str) -> color_eyre::Result<String> {
            let lua = Lua::new();
            let printout = Arc::new(RwLock::new(String::new()));
            {
                let globals = lua.globals();
                $(
                    let $ext = lua.create_table()?;
                    $({
                        let p = Arc::clone(rpmparser);
                        $ext.set(stringify!($name), lua.create_function(move |lua, arg| $ext::$name(&p, lua, arg))?)?;
                    })+
                    globals.set(stringify!($ext), $ext)?;
                )+
                let printout = Arc::clone(&printout);
                globals.set(
                    "print",
                    lua.create_function(move |_, s: String| {
                        printout.write().push_str(&s);
                        Ok(())
                    })?,
                )?;
                lua.load(script).exec().map_err(|e| color_eyre::eyre::eyre!("Cannot execute script: {script}").wrap_err(e))?;
            }
            drop(lua);
            Ok(Arc::try_unwrap(printout).expect("Cannot unwrap Arc for print() output in lua").into_inner())
        }
    };
}

__lua!(
    mod rpm {
        fn b64decode(_, _, arg): String {
            String::from_utf8(STANDARD.decode(arg).map_err(ExternalError::into_lua_err)?).map_err(ExternalError::into_lua_err)
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
                p.write().macros.insert(name.into(), vec![(*def).into()]);
                Ok(())
            } else {
                Err("Invalid syntax: `%define {def}`".into_lua_err())
            }
        }
        fn execute(_, _, args=>Vec<String>): i32 {
            Ok(std::process::Command::new(&args[0]).args(&args[1..]).status().map_err(mlua::ExternalError::into_lua_err)?.code().unwrap_or(-1))
        }
        fn expand(p, _, arg): String {
            let mut out = smartstring::SmartString::new();
            p.write().parse_macro::<std::fs::File>(&mut out, &mut (&*arg).into()).map_err(mlua::ExternalError::into_lua_err)?;
            Ok(out.to_string())
        }
        // glob(_, _, arg=>(String, Option<String>))
        fn interactive(_, _, _=>()) {
            super::repl(); // lazy
            // todo mimic
            Ok(())
        }
        fn isdefined(p, _, name): (bool, bool) {
            if let Some(def) = p.read().macros.get(&*name) {
                if let Some(MacroType::Runtime { param, .. }) = def.last() {
                    return Ok((true, *param));
                }
            }
            Ok((false, false))
        }
        fn load(p, _, arg) {
            p.write().load_macro_from_file(&std::path::PathBuf::from(arg)).map_err(mlua::ExternalError::into_lua_err)
        }
        fn open(_, _, args=>(String, Option<String>)): super::RpmFile {
            let (path, mode) = args;
            super::RpmFile::new(path, mode.as_ref().map_or("+", |s| s))
        }
        fn undefine(p, _, name) {
            p.write().macros.remove(&*name).ok_or_else(|| "error undefining macro".into_lua_err())?;
            Ok(())
        }
        fn vercmp(_, _, vers=>(String, String)): i8 {
            use rpmspec_common::expr::Version;
            use std::str::FromStr;
            let (v1, v2) = vers;
            let (v1, v2) = (Version::from_str(&v1).map_err(ExternalError::into_lua_err)?, Version::from_str(&v2).map_err(ExternalError::into_lua_err)?);
            Ok(v1.cmp(&v2) as i8)
        }
    }
    mod posix {
        fn access(_, _, args=>(String, Option<String>)): bool {
            use std::os::unix::fs::PermissionsExt;
            let (path, mode) = args;
            let mode = mode.as_ref().map_or("f", std::string::String::as_str);
            let p = std::path::Path::new(&path);
            if mode.contains('f') && !p.exists(){
                return Ok(false);
            }
            let perms = p.metadata().map_err(ExternalError::into_lua_err)?.permissions().mode();
            let x = perms & 0o7;
            let w = (perms >> 3) & 0o7;
            let r = (perms >> 6) & 0o7;
            if mode.contains('r') && r == 0 { return Ok(false); }
            if mode.contains('w') && w == 0 { return Ok(false); }
            if mode.contains('x') && x == 0 { return Ok(false); }
            Ok(true)
        }
        fn chdir(_, _, dir) {
            std::env::set_current_dir(dir).map_err(ExternalError::into_lua_err)
        }
        fn chmod(_, _, args=>(String, String)) {
            std::process::Command::new("chmod").arg(args.1).arg(args.0).status().map_err(ExternalError::into_lua_err)?;
            Ok(())
        }
        fn chown(_, _, args=>(String, String, String)) {
            let (path, user, group) = args;
            std::process::Command::new("chown").arg(format!("{user}:{group}")).arg(path).status().map_err(ExternalError::into_lua_err)?;
            Ok(())
        }
        fn ctermid(_, _, _=>()): String {
            // SAFETY: mado dunno??
            // well it's just pointer arithmetic + string manipulations
            let cstr = unsafe {
                // ctermid() can pass in null for current terminal
                let out = libc::ctermid(std::ptr::null_mut());
                CStr::from_ptr(out)
            };
            cstr.to_str().map_err(ExternalError::into_lua_err).map(ToOwned::to_owned)
        }
        fn dir(_, _, dir=>Option<String>): Vec<String> {
            let dir = match dir {
                Some(dir) => std::path::PathBuf::from(dir),
                None => std::env::current_dir().map_err(ExternalError::into_lua_err)?,
            };
            let rd = std::fs::read_dir(dir).map_err(ExternalError::into_lua_err)?;
            let mut res = vec![];
            for f in rd {
                let f = f.map_err(ExternalError::into_lua_err)?;
                res.push(f.path().to_string_lossy().to_string());
            }
            Ok(res)
        }
        fn errno(_, _, _=>()): (String, isize) {
            // FIXME: no idea how to get errno… defaulting to 0
            unsafe {
                core::ffi::CStr::from_ptr(libc::strerror(0))
            }.to_str().into_lua_err().map(ToOwned::to_owned).map(|s| (s, 0))
        }
        fn exec(_, _, _=>Vec<String>) {
            Err("This deprecated function will be removed in rpm 6.0, please use rpm.execute() instead.").into_lua_err()
        }
        fn files(p, ctx, f=>Option<String>): Vec<String> {
            dir(p, ctx, f)
        }
        fn fork(_, _, _=>()): isize {
            Err("This deprecated function will be removed in rpm 6.0, please use rpm.execute() instead.").into_lua_err()
        }
        fn getcwd(_, _, _=>()): String {
            std::env::current_dir().map_err(ExternalError::into_lua_err).map(|p| p.to_string_lossy().to_string())
        }
        fn getenv(_, _, name=>String): String {
            std::env::var(name).map_err(ExternalError::into_lua_err)
        }
        fn getgroup<'lua>(_, lua, group=>mlua::Value): mlua::Table<'lua> {
            let g = match group {
                mlua::Value::Number(n) => unsafe {
                    libc::getgrgid(n.round() as u32)
                },
                mlua::Value::String(s) => unsafe {
                    libc::getgrnam(CString::new(s.to_str()?).into_lua_err()?.as_ptr())
                },
                _ => return Err(mlua::Error::BadArgument {
                    to: Some("getgroup".into()),
                    pos: 1,
                    name: Some("group".into()),
                    cause: "Only accepts Number/String".into_lua_err().into()
                }),
            };
            if g.is_null() {
                lua.create_table()
            } else {
                let table = lua.create_table()?;
                unsafe {
                    table.set("name", core::ffi::CStr::from_ptr((*g).gr_name).to_str().into_lua_err()?)?;
                    table.set("gid", (*g).gr_gid)?;
                }
                Ok(table)
            }
        }
        fn getlogin(_, _, _=>()): String {
            unsafe { core::ffi::CStr::from_ptr(libc::getlogin()).to_str().into_lua_err().map(ToOwned::to_owned) }
        }
        fn getpasswd<'lua>(_, lua, args=>(Option<mlua::Value>, Option<String>)): Value<'lua> {
            let (user, selector) = args;
            let p = match user {
                None => unsafe { libc::getpwuid(libc::geteuid()) },
                Some(mlua::Value::Number(n)) => unsafe { libc::getpwuid(n.round() as u32) },
                Some(mlua::Value::String(s)) => unsafe {
                    libc::getpwnam(CString::new(s.to_str()?).into_lua_err()?.as_ptr())
                },
                _ => return Err(mlua::Error::BadArgument {
                    to: Some("getpasswd".into()),
                    pos: 1,
                    name: Some("user".into()),
                    cause: "Only accepts optional Number/String".into_lua_err().into()
                }),
            };
            if p.is_null() {
                return Err("libc::getpwuid() or libc::getpwnam() returned nullptr; cannot getpasswd()").into_lua_err();
            }
            let p = unsafe { *p };
            let Some(select) = selector else {
                let t = lua.create_table()?;
                unsafe {
                    t.set("name", CStr::from_ptr(p.pw_name).to_str().into_lua_err()?)?;
                    t.set("uid", p.pw_uid)?;
                    t.set("gid", p.pw_gid)?;
                    t.set("dir", CStr::from_ptr(p.pw_dir).to_str().into_lua_err()?)?;
                    t.set("shell", CStr::from_ptr(p.pw_shell).to_str().into_lua_err()?)?;
                    t.set("gecos", CStr::from_ptr(p.pw_gecos).to_str().into_lua_err()?)?;
                    t.set("passwd", CStr::from_ptr(p.pw_passwd).to_str().into_lua_err()?)?;
                }
                return Ok(Value::Table(t));
            };
            Ok(match &*select {
                "name" => Value::String(unsafe { lua.create_string(CStr::from_ptr(p.pw_name).to_bytes())? }),
                "uid" => Value::Number(f64::from(p.pw_uid)),
                "gid" => Value::Number(f64::from(p.pw_gid)),
                "dir" => Value::String(unsafe { lua.create_string(CStr::from_ptr(p.pw_dir).to_bytes())? }),
                "shell" => Value::String(unsafe { lua.create_string(CStr::from_ptr(p.pw_shell).to_bytes())? }),
                "gecos" => Value::String(unsafe { lua.create_string(CStr::from_ptr(p.pw_gecos).to_bytes())? }),
                "passwd" => Value::String(unsafe { lua.create_string(CStr::from_ptr(p.pw_passwd).to_bytes())? }),
                _ => return Err(mlua::Error::BadArgument {
                    to: Some("getpasswd".into()),
                    pos: 1,
                    name: Some("selector".into()),
                    cause: "Only accepts optional 'name'/'uid'/'gid'/'dir'/'shell'/'gecos'/'passwd'".into_lua_err().into()
                }),
            })
        }
        fn getprocessid<'lua>(_, lua, arg=>Option<String>): mlua::Value<'lua> {
            let Some(arg) = arg else {
                let t = lua.create_table()?;
                unsafe {
                    t.set("egid", libc::getegid())?;
                    t.set("euid", libc::geteuid())?;
                    t.set("gid", libc::getgid())?;
                    t.set("uid", libc::getuid())?;
                    t.set("pgrp", libc::getpgrp())?;
                    t.set("pid", libc::getpid())?;
                    t.set("ppid", libc::getppid())?;
                }
                return Ok(mlua::Value::Table(t));
            };
            Ok(mlua::Value::Number(unsafe {
                match &*arg {
                    "egid" => f64::from(libc::getegid()),
                    "euid" => f64::from(libc::geteuid()),
                    "gid" => f64::from(libc::getgid()),
                    "uid" => f64::from(libc::getuid()),
                    "pgrp" => f64::from(libc::getpgrp()),
                    "pid" => f64::from(libc::getpid()),
                    "ppid" => f64::from(libc::getppid()),
                    _ => return Err("Invalid argument to getprocessid()").into_lua_err(),
                }
            }))
        }
        fn kill(_, _, (pid, signal)=>(i32, Option<i32>)): i32 {
            let signal = signal.unwrap_or(15); // SIGTERM
            Ok(unsafe { libc::kill(pid, signal) })
        }
        fn link(_, _, (old, new)=>(String, String)): i32 {
            let old = CString::new(old).into_lua_err()?.as_ptr();
            let new = CString::new(new).into_lua_err()?.as_ptr();
            Ok(unsafe { libc::link(old, new)})
        }
        fn mkdir(_, _, path) {
            std::fs::create_dir(path).map_err(ExternalError::into_lua_err)
        }
        fn mkfifo(_, _, path): i32 {
            Ok(unsafe { libc::mkfifo(CString::new(path).into_lua_err()?.as_ptr(), 0o777) })
        }
        // pathconf() return type???
        fn putenv(_, _, kv) {
            let (key, value) = kv.split_once('=').ok_or_else(|| "putenv(): Cannot find `=` in provided argument".into_lua_err())?;
            std::env::set_var(key, value);
            Ok(())
        }
        fn readlink(_, _, path=>String): String {
            let mut cbuf = Vec::with_capacity(512);
            unsafe {
                libc::readlink(CString::new(path).into_lua_err()?.as_ptr(), cbuf.as_mut_ptr(), 512);
            }
            unsafe { CStr::from_ptr(cbuf.as_ptr().cast()) }.to_str().map(ToOwned::to_owned).into_lua_err()
        }
        fn rmdir(_, _, path) {
            std::fs::remove_dir(path).map_err(ExternalError::into_lua_err)
        }
        fn setgid(_, _, arg=>mlua::Value): i32 {
            let gid = match arg {
                mlua::Value::Number(n) => n.round() as u32,
                mlua::Value::String(s) => unsafe {
                    let p = libc::getgrnam(CString::new(s.to_str()?).into_lua_err()?.as_ptr());
                    if p.is_null() {
                        return Err("libc::getgrnam() returned nullptr; can't get gid").into_lua_err();
                    }
                    (*p).gr_gid
                },
                _ => return Err("Bad argument type to setgid(), accepts String/Number").into_lua_err(),
            };
            Ok(unsafe { libc::setgid(gid) })
        }
        fn setuid(_, _, arg=>mlua::Value): i32 {
            let uid = match arg {
                mlua::Value::Number(n) => n.round() as u32,
                mlua::Value::String(s) => unsafe {
                    let p = libc::getpwnam(CString::new(s.to_str()?).into_lua_err()?.as_ptr());
                    if p.is_null() {
                        return Err("libc::getpwnam() returned nullptr; can't get uid").into_lua_err();
                    }
                    (*p).pw_uid
                },
                _ => return Err("Bad argument type to setuid(), accepts String/Number").into_lua_err(),
            };
            Ok(unsafe { libc::setuid(uid) })
        }
        fn sleep(_, _, seconds=>u64) {
            std::thread::sleep(std::time::Duration::from_secs(seconds));
            Ok(())
        }
        // stat() return type???
        fn symlink(_, _, paths=>(String, String)) {
            let (path1, path2) = paths;
            let path1 = CString::new(path1).map_err(ExternalError::into_lua_err)?;
            let path2 = CString::new(path2).map_err(ExternalError::into_lua_err)?;
            if unsafe { libc::symlink(path1.as_ptr(), path2.as_ptr()) } != 0 {
                return Err("libc::symlink() returned -1".into_lua_err())
            }
            Ok(())
        }
        // sysconf() return type???
        // times() return type???
        fn ttyname(_, _, fd=>Option<i32>): String {
            Ok(unsafe { CStr::from_ptr(libc::ttyname(fd.unwrap_or(0))) }.to_string_lossy().to_string())
        }
        fn umask(_, _, arg=>mlua::Value): String {
            let mode = match arg {
                mlua::Value::Number(n) => umask::Mode::from({
                    let mut n = n.round() as u32;
                    let hundred = n / 100;
                    let mut oct = hundred * 0o100;
                    n -= hundred * 100;
                    let tenth = n / 10;
                    oct += tenth * 0o10;
                    n -= tenth * 10;
                    oct += n;
                    if oct > 0o777 {
                        return Err("oct value passed into umask() exceeds 0o777").into_lua_err();
                    }
                    oct
                }),
                mlua::Value::String(s) => s.to_str()?.parse().into_lua_err()?,
                mlua::Value::Nil => umask::Mode::from(0),
                _ => return Err("Bad argument type to umask(), accepts optional String/Number").into_lua_err(),
            };
            Ok(umask::Mode::from(unsafe { libc::umask(mode.into()) }).to_string())

        }
        fn uname(_, _, format): String {
            let u = std::ptr::null_mut();
            if unsafe { libc::uname(u) } == -1 {
                return Err("libc::uname() returned -1").into_lua_err();
            }
            let u = unsafe { *u };
            let mut res = String::new();
            let mut chars = format.chars();
            while let Some(c) = chars.next() {
                if c == '%' {
                    let Some(next) = chars.next() else {
                        res.push('%');
                        return Ok(res);
                    };
                    match next {
                        '%' => res.push('%'),
                        'm' => res.push_str(unsafe { CStr::from_ptr(u.machine.as_ptr()) }.to_str().into_lua_err()?),
                        'n' => res.push_str(unsafe { CStr::from_ptr(u.nodename.as_ptr()) }.to_str().into_lua_err()?),
                        'r' => res.push_str(unsafe { CStr::from_ptr(u.release.as_ptr()) }.to_str().into_lua_err()?),
                        's' => res.push_str(unsafe { CStr::from_ptr(u.sysname.as_ptr()) }.to_str().into_lua_err()?),
                        'v' => res.push_str(unsafe { CStr::from_ptr(u.version.as_ptr()) }.to_str().into_lua_err()?),
                        _ => return Err(format!("Bad format option `%{next}` for uname()")).into_lua_err(),
                    }
                    continue;
                }
                res.push(c);
            }
            Ok(res)
        }
        fn utime(_, _, (path, mtime, ctime)=>(String, Option<i64>, Option<i64>)): i32 {
            let currtime = unsafe { libc::time(std::ptr::null_mut()) };
            let modtime = mtime.unwrap_or(currtime);
            let actime = ctime.unwrap_or(currtime);
            let times = libc::utimbuf { actime, modtime };
            Ok(unsafe { libc::utime(
                CString::new(path).into_lua_err()?.as_ptr(),
                std::ptr::from_ref(&times)
            )})
        }
        fn wait(_, _, _=>usize) {
            Err("This deprecated function will be removed in rpm 6.0, please use rpm.execute() instead.").into_lua_err()
        }
        fn setenv(_, _, args=>(String, String, Option<bool>)) {
            let (name, value, should_override) = args;
            if should_override.unwrap_or(true) || matches!(std::env::var(&name), Err(std::env::VarError::NotPresent)) {
                std::env::set_var(name, value);
            }
            Ok(())
        }
        fn unsetenv(_, _, key) {
            std::env::remove_var(key);
            Ok(())
        }
    }
);
