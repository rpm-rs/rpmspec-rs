//! Decompression
//!
//! https://github.com/rpm-software-management/rpm/blob/master/tools/rpmuncompress.c

use std::{io::Read, path::Path};

/// Compression formats identified
pub enum CmprxFmt {
	/// .bzip2
	BZIP2,
	/// .zip
	ZIP,
	/// .xz
	XZ,
	/// .zstd
	ZSTD,
	/// .lzip
	LZIP,
	/// .lrzip
	LRZIP,
	/// gzip, old gzip, pack, SCO lzh, compress
	Other,
	/// .7z
	SEVENZIP, // oh well
	/// .lzma
	LZMA,
	/// .gem
	GEM,
	/// not identified
	Nil,
}

impl TryFrom<&Path> for CmprxFmt {
	type Error = std::io::Error;
	fn try_from(value: &Path) -> Result<Self, Self::Error> {
		let mut buf: [u8; 6] = [0; 6];
		macro_rules! magic {
			($x:ident: $($c:expr)+) => {
				if buf.starts_with(&[$($c as u8, )+]) {
					return Ok(Self::$x);
				}
			};
		}
		let mut f = std::fs::File::open(value)?;
		f.read_exact(&mut buf)?;
		magic!(BZIP2: 'B' 'Z' 'h');
		magic!(ZIP: 'P' 'K' 3 4);
		magic!(ZIP: 'P' 'K' 0 0); // pkzip
		magic!(XZ: 0xfd 0x37 0x7a 0x58 0x5a 0x00); // new style xz (lzma) with magic
		magic!(ZSTD: 0x28 0x85 0x2f);
		magic!(LZIP: 'L' 'Z' 'I' 'P');
		magic!(LRZIP: 'L' 'R' 'Z' 'I');
		magic!(Other: 0037 0213); // gzip
		magic!(Other: 0037 0236); // old gzip
		magic!(Other: 0037 0036); // pack
		magic!(Other: 0037 0240); // SCO lzh
		magic!(Other: 0037 0235); // compress
		magic!(SEVENZIP: '7' 'z' 0xbc 0xaf 0x27 0x1c);
		if let Some(Some(ext)) = value.extension().map(|x| x.to_str()) {
			if ext == "lzma" || ext == "LZMA" {
				return Ok(Self::LZMA);
			}
			if ext == "gem" || ext == "GEM" {
				return Ok(Self::GEM);
			}
		}
		return Ok(Self::Nil);
	}
}
