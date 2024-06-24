/// Populate preamble-related stuff
#[macro_export]
macro_rules! preamble_maker {
    (
        $(#[$pattr:meta])*
        $RPMSpecPkg:ident {
            $(
                $(#[$fpattr:meta])*
                $fieldp:ident: $typ:ident
            ),*$(,)?;;$($ptokens:tt)*
        }
        $(#[$attr:meta])*
        $RPMSpec:ident {
            $(
                $(#[$fattr:meta])*
                $field:ident: $ty:ident
            ),*$(,)?;;$($tokens:tt)*
        }
    ) => { ::paste::paste! {
        // build the structs
        $(#[$pattr])*
        pub struct $RPMSpecPkg {
            $(
                $(#[$fpattr])*
                #[doc = "Represents `" $fieldp ":`"]
                pub [<$fieldp:lower>]: $typ,
            )*
            $($ptokens)*
        }
        $(#[$attr])*
        pub struct $RPMSpec {
            $(
                $(#[$fattr])*
                #[doc = "Represents `" $field ":`"]
                pub [<$field:lower>]: $ty,
            )*
            $($tokens)*
        }

        // create macros for populating preamble parsing fns
        macro_rules! preamble_pkg_parser {
            ($self:ident $rpm:ident $pkg:ident $name:ident $value:ident $offset:ident $csm:ident) => {
                $(
                    const [<__PREAMBLE_MAKER_P_ $fieldp:upper>]: &str = stringify!($fieldp);
                )*

                let $rpm = $rpm.packages.get_mut($pkg).expect("no subpkgs in rpm.packages");

                match $name {
                    $(
                        [<__PREAMBLE_MAKER_P_ $fieldp:upper>] => {
                            $crate::preamble_maker!(@p($self $rpm $pkg $name $value $offset $csm) $fieldp: $typ);
                        },
                    )*
                    _ => {}, // get to global
                }
            };
        }
        macro_rules! preamble_parser {
            ($self:ident $name:ident $value:ident $offset:ident $csm:ident) => {{
                $(
                    const [<__PREAMBLE_MAKER_G_ $field:upper>]: &str = stringify!($field);
                )*

                // ::tracing::debug!("Adding preamble");
                let rpm = &mut $self.rpm;

                if let RPMSection::Package(ref pkg) = $self.section {
                    preamble_pkg_parser!($self rpm pkg $name $value $offset $csm);
                }

                match $name {
                    $(
                        [<__PREAMBLE_MAKER_G_ $field:upper>] => {
                            $crate::preamble_maker!(@g($self rpm $name $value $offset $csm) $field: $ty);
                        },
                    )*
                    _ => $self.errors.push(::rpmspec_common::PErr::UnknownPreamble(0, $name.into())),
                }
                Ok(())
            }};
        }
    }};
    (@g($self:ident $rpm:ident $name:ident $value:ident $offset:ident $csm:ident) $field:ident: OptString) => { ::paste::paste! {
        if let Some(ref old) = $rpm.[<$field:lower>] {
            ::tracing::warn!("overriding existing {} preamble value `{old}` to `{}`", stringify!($field), $value);
        }
        let m = MacroType::Runtime { s: $csm.s.clone(), file: $csm.file.clone(), offset: $offset, param: false, len: $value.len() };
        if let Some(v) = $self.macros.get_mut(stringify!([<$field:lower>])) {
            v.push(m);
        } else {
            $self.macros.insert(stringify!([<$field:lower>]).into(), vec![m]);
        }
        $rpm.[<$field:lower>] = Some($value);
    }};
    (@g($self:ident $rpm:ident $name:ident $value:ident $offset:ident $csm:ident) $field:ident: bool) => { ::paste::paste! {
        $rpm.[<$field:lower>] = $value.parse()?;
        return Ok(());
    }};
    (@g($self:ident $rpm:ident $name:ident $value:ident $offset:ident $csm:ident) $field:ident: Strings) => { ::paste::paste! {
        $rpm.[<$field:lower>].extend($value.split_whitespace().map_into());
        return Ok(());
    }};
    (@g($self:ident $rpm:ident $name:ident $value:ident $offset:ident $csm:ident) Epoch: u32) => {
        $rpm.epoch = $value.parse().map_err(|e: ::std::num::ParseIntError| eyre!(e).wrap_err("Failed to decode epoch to int"))?;
        return Ok(());
    };
    (@g($self:ident $rpm:ident $name:ident $value:ident $offset:ident $csm:ident) $field:ident: Pkgs) => { ::paste::paste! {
        return Package::add_query(&mut $rpm.[<$field:lower>], &$value)
    }};
    (@g($self:ident $rpm:ident $name:ident $value:ident $offset:ident $csm:ident) $field:ident: SimplePkgs) => { ::paste::paste! {
        return Package::add_simple_query(&mut $rpm.[<$field:lower>], &$value)
    }};
    (@p($self:ident $rpm:ident $pkg:ident $name:ident $value:ident $offset:ident $csm:ident) $fieldp:ident: OptString) => { ::paste::paste! {
        if let Some(ref old) = $rpm.[<$fieldp:lower>] {
            ::tracing::warn!("overriding existing {} preamble value `{old}` to `{}`", stringify!($field), $value);
        }
        $rpm.[<$fieldp:lower>] = Some($value);
        return Ok(());
    }};
    (@p($self:ident $rpm:ident $pkg:ident $name:ident $value:ident $offset:ident $csm:ident) $fieldp:ident: Pkgs) => { ::paste::paste! {
        return Package::add_query(&mut $rpm.[<$fieldp:lower>], &$value)
    }};
    (@p($self:ident $rpm:ident $pkg:ident $name:ident $value:ident $offset:ident $csm:ident) $fieldp:ident: SimplePkgs) => { ::paste::paste! {
        return Package::add_simple_query(&mut $rpm.[<$fieldp:lower>], &$value)
    }};
}
