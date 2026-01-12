use clap::Parser;
use quick_xml::{
    Reader,
    events::{BytesStart, Event},
};
use std::{fmt::Write, path::PathBuf};

#[derive(Parser)]
struct Args {
    input: PathBuf,
}

fn if_static_name(if_name: &str) -> String {
    let mut x = if_name.to_uppercase();
    x.push_str("_INTERFACE");
    x
}

fn if_name_to_type_name(if_name: &str) -> String {
    let mut needs_upper = true;
    let mut sink = String::with_capacity(if_name.len());
    for c in if_name.chars() {
        if c == '_' {
            needs_upper = true;
            continue;
        }

        if needs_upper {
            sink.extend(c.to_uppercase());
            needs_upper = false;
        } else {
            sink.push(c);
        }
    }

    sink
}

fn enum_type_name(n: &str) -> String {
    let mut needs_upper = true;
    let mut sink = String::with_capacity(n.len());
    for c in n.chars() {
        if c == '_' {
            needs_upper = true;
            continue;
        }

        if needs_upper {
            sink.extend(c.to_uppercase());
            needs_upper = false;
        } else {
            sink.push(c);
        }
    }

    sink
}

fn wrapper_enum_type_name(n: &str, callsite_type_name: &str) -> String {
    if n.contains('.') {
        // fully qualified
        let mut sink = String::with_capacity(n.len());
        let mut needs_upper = true;
        for c in n.chars() {
            if c == '_' {
                needs_upper = true;
                continue;
            }
            if c == '.' {
                needs_upper = true;
                continue;
            }

            if needs_upper {
                sink.extend(c.to_uppercase());
                needs_upper = false;
            } else {
                sink.push(c);
            }
        }

        return sink;
    }

    // should prefixed with callsite_type_name
    let mut sink = String::with_capacity(n.len() + callsite_type_name.len());
    sink.push_str(callsite_type_name);
    let mut needs_upper = true;
    for c in n.chars() {
        if c == '_' {
            needs_upper = true;
            continue;
        }

        if needs_upper {
            sink.extend(c.to_uppercase());
            needs_upper = false;
        } else {
            sink.push(c);
        }
    }

    sink
}

fn enum_entry_name(n: &str) -> String {
    let mut needs_upper = true;
    let mut sink = String::with_capacity(n.len());
    for c in n.chars() {
        if c == '_' {
            needs_upper = true;
            continue;
        }

        if needs_upper {
            sink.extend(c.to_uppercase());
            needs_upper = false;
        } else {
            sink.push(c);
        }
    }

    sink
}

fn if_name_to_typeref(if_name: &str) -> String {
    if let Some(s) = if_name.strip_prefix("wl_") {
        // special case for base interfaces
        return format!("crate::{}", if_name_to_type_name(s));
    }

    format!("crate::{}", if_name_to_type_name(if_name))
}

enum RsIdent<'r> {
    Const(&'static str),
    Generated(String),
    Ref(&'r str),
}
impl PartialEq for RsIdent<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}
impl Eq for RsIdent<'_> {}
impl RsIdent<'_> {
    fn as_str<'s>(&'s self) -> &'s str {
        match self {
            Self::Const(x) => x,
            Self::Generated(x) => x,
            Self::Ref(x) => x,
        }
    }

    fn requires_escape(&self) -> bool {
        matches!(self.as_str(), "move" | "type")
    }
}
impl core::fmt::Display for RsIdent<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.requires_escape() {
            f.write_str("r#")?;
        }

        f.write_str(self.as_str())
    }
}

enum RsPathElement<'r> {
    Crate,
    Ident(RsIdent<'r>),
}
impl<'r> From<RsIdent<'r>> for RsPathElement<'r> {
    #[inline(always)]
    fn from(value: RsIdent<'r>) -> Self {
        Self::Ident(value)
    }
}

struct RsPath<'r> {
    pub parent: Option<Box<RsPath<'r>>>,
    pub element: RsPathElement<'r>,
}
impl<'r> RsPath<'r> {
    const CRATE: Self = Self {
        parent: None,
        element: RsPathElement::Crate,
    };

    fn descendant(self, e: impl Into<RsPathElement<'r>>) -> Self {
        Self {
            parent: Some(Box::new(self)),
            element: e.into(),
        }
    }
}
impl core::fmt::Display for RsPath<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref p) = self.parent {
            p.fmt(f)?;
            f.write_str("::")?;
        }

        match self.element {
            RsPathElement::Crate => f.write_str("crate"),
            RsPathElement::Ident(ref x) => x.fmt(f),
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let content = std::fs::read_to_string(&args.input)?;
    let mut reader = Reader::from_str(&content);
    let proto = xml_document(&mut reader)?;
    // println!("{proto:#?}");

    // println!("** {}.rs", proto.name);
    if let Some(ref d) = proto.description {
        println!("//! {}: {}", proto.name, d.summary);
        let lines = d.content.lines().collect::<Vec<_>>();
        if lines.len() > 1 {
            let common_prefix = lines
                .iter()
                .skip(1)
                .fold(None, |prefix, &l| {
                    if l.trim().is_empty() {
                        // not account for common-prefix checking
                        return prefix;
                    }

                    match prefix {
                        None => Some(l),
                        Some(prefix) => {
                            let common_prefix_bytes = prefix
                                .bytes()
                                .zip(l.bytes())
                                .take_while(|&(a, b)| a == b)
                                .count();
                            Some(unsafe { l.get_unchecked(..common_prefix_bytes) })
                        }
                    }
                })
                .unwrap_or("");

            for l in lines {
                println!("//! {}", l.strip_prefix(common_prefix).unwrap_or(l));
            }
        }
        println!("");
    }

    println!("use crate::{{ffi, Proxy, Interface}};");
    println!("");

    for x in proto.interfaces.iter() {
        let if_static_var_name = RsIdent::Generated(if_static_name(&x.name));
        let type_name = RsIdent::Generated(if_name_to_type_name(&x.name));
        let event_listener_trait_name =
            RsIdent::Generated(format!("{}EventListener", type_name.as_str()));

        let mut if_request_messages = String::new();
        let mut request_wrappers = String::new();
        let mut destructor = None;
        for (n, r) in x.requests.iter().enumerate() {
            let request_name = &r.name;
            let request_name_ident = RsIdent::Ref(request_name);

            let mut type_chars = String::with_capacity(r.args.len());
            let mut if_pointers = String::new();
            if let Some(v) = r.since {
                let _ = write!(type_chars, "{v}");
            }
            let mut newid_if = None;
            let mut wrapper_args = String::new();
            let mut marshal_args = String::new();
            for a in r.args.iter() {
                if a.r#type == WlWireFormatType::NewID {
                    if newid_if.is_some() {
                        panic!("too many new_id args");
                    }

                    newid_if = Some(a.interface.as_deref().expect("new_id without interface?"));
                }

                match a.interface {
                    Some(ref t) if t == &x.name => {
                        let _ = write!(if_pointers, "&{if_static_var_name} as *const _,");
                    }
                    Some(ref t) => {
                        let def_ref = if let Some(s) = t.strip_prefix("wl_") {
                            // special case for base interfaces
                            RsPath::CRATE.descendant(RsIdent::Generated(if_name_to_type_name(s)))
                        } else {
                            RsPath::CRATE.descendant(RsIdent::Generated(if_name_to_type_name(t)))
                        };
                        let def_ref = RsPath {
                            parent: Some(Box::new(def_ref)),
                            element: RsPathElement::Ident(RsIdent::Const("DEF")),
                        };

                        let _ = write!(if_pointers, "{def_ref},");
                    }
                    None => if_pointers.push_str("core::ptr::null(),"),
                }

                if a.allow_null {
                    type_chars.push('?');
                }

                type_chars.push(a.r#type.signature_char());

                let arg_name_ident = RsIdent::Ref(&a.name);
                match (
                    a.r#type,
                    a.interface.as_deref(),
                    a.r#enum.as_deref(),
                    a.allow_null,
                ) {
                    (WlWireFormatType::UInt, None, None, false) => {
                        let _ = write!(wrapper_args, "{arg_name_ident}: u32,");
                        let _ = write!(marshal_args, "ffi::Argument {{ u: {arg_name_ident} }},");
                    }
                    (WlWireFormatType::UInt, None, Some(t), false) => {
                        let _ = write!(
                            wrapper_args,
                            "{arg_name_ident}: {},",
                            wrapper_enum_type_name(t, type_name.as_str())
                        );
                        let _ = write!(marshal_args, "{arg_name_ident}.as_arg(),");
                    }
                    (WlWireFormatType::Int, None, None, false) => {
                        let _ = write!(wrapper_args, "{arg_name_ident}: i32,");
                        let _ = write!(marshal_args, "ffi::Argument {{ i: {arg_name_ident} }},");
                    }
                    (WlWireFormatType::Int, None, Some(t), false) => {
                        let _ = write!(
                            wrapper_args,
                            "{arg_name_ident}: {type_name}{},",
                            enum_type_name(t)
                        );
                        let _ = write!(
                            marshal_args,
                            "ffi::Argument {{ i: {arg_name_ident} as _ }},"
                        );
                    }
                    (WlWireFormatType::String, None, None, false) => {
                        let _ = write!(wrapper_args, "{arg_name_ident}: &core::ffi::CStr,");
                        let _ = write!(
                            marshal_args,
                            "ffi::Argument {{ s: {arg_name_ident}.as_ptr() }},"
                        );
                    }
                    (WlWireFormatType::Object, None, None, false) => {
                        let _ = write!(wrapper_args, "{arg_name_ident}: &Proxy,");
                        let _ = write!(marshal_args, "{arg_name_ident}.as_arg(),");
                    }
                    (WlWireFormatType::Object, Some(x), None, false) => {
                        let _ = write!(
                            wrapper_args,
                            "{arg_name_ident}: &{},",
                            if_name_to_typeref(x)
                        );
                        let _ = write!(marshal_args, "{arg_name_ident}.0.as_arg(),");
                    }
                    (WlWireFormatType::Object, None, None, true) => {
                        let _ = write!(wrapper_args, "{arg_name_ident}: Option<&Proxy>,");
                        let _ = write!(
                            marshal_args,
                            "{arg_name_ident}.map_or(crate::NULLOBJ_ARG, Proxy::as_arg),"
                        );
                    }
                    (WlWireFormatType::Object, Some(x), None, true) => {
                        let _ = write!(
                            wrapper_args,
                            "{arg_name_ident}: Option<&{}>,",
                            if_name_to_typeref(x)
                        );
                        let _ = write!(
                            marshal_args,
                            "{arg_name_ident}.map_or(crate::NULLOBJ_ARG, |x| x.0.as_arg()),"
                        );
                    }
                    (WlWireFormatType::NewID, _, None, false) => {
                        // new_id does not appear in wrapper_args(return position)
                        let _ = write!(marshal_args, "crate::NEWID_ARG,");
                    }
                    (WlWireFormatType::Fixed, None, None, false) => {
                        let _ = write!(wrapper_args, "{arg_name_ident}: crate::Fixed,");
                        let _ = write!(marshal_args, "ffi::Argument {{ f: {arg_name_ident} }},");
                    }
                    (WlWireFormatType::UInt, _, _, true)
                    | (WlWireFormatType::UInt, Some(_), _, _)
                    | (WlWireFormatType::Int, _, _, true)
                    | (WlWireFormatType::Int, Some(_), _, _)
                    | (WlWireFormatType::Fixed, _, _, true)
                    | (WlWireFormatType::Fixed, Some(_), _, _)
                    | (WlWireFormatType::Fixed, _, Some(_), _)
                    | (WlWireFormatType::FD, _, _, true)
                    | (WlWireFormatType::FD, Some(_), _, _)
                    | (WlWireFormatType::FD, _, Some(_), _)
                    | (WlWireFormatType::NewID, None, _, _)
                    | (WlWireFormatType::String, Some(_), _, _)
                    | (WlWireFormatType::String, _, Some(_), _)
                    | (WlWireFormatType::Object, _, Some(_), _) => {
                        unreachable!(
                            "invalid pattern: {:?} {:?} {:?} {}",
                            a.interface, a.r#enum, a.r#type, a.allow_null
                        )
                    }
                    _ => todo!(
                        "wrapper/marshal arg: {:?} {:?} {:?} {}",
                        a.r#type,
                        a.interface,
                        a.r#enum,
                        a.allow_null
                    ),
                }
            }

            let _ = write!(
                if_request_messages,
                r#"ffi::Message {{ name: c"{request_name}".as_ptr(), signature: c"{type_chars}".as_ptr(), types: const {{ [{if_pointers}] }}.as_ptr() }},"#,
            );

            if r.r#type.as_deref().is_some_and(|x| x == "destructor") {
                if destructor.is_some() {
                    panic!("multiple destructor");
                }

                destructor = Some((n, r.args.is_empty(), r.since));
            } else {
                let _ = write!(
                    request_wrappers,
                    "    #[inline] pub fn {request_name_ident}(&self,{wrapper_args}) -> "
                );
                if let Some(x) = newid_if {
                    let _ = write!(
                        request_wrappers,
                        "crate::Result<crate::Owned<{}>>",
                        if_name_to_typeref(x)
                    );
                } else {
                    request_wrappers.push_str("crate::Result<()>");
                }
                let _ = writeln!(request_wrappers, " {{");
                if newid_if.is_some() {
                    let _ = writeln!(
                        request_wrappers,
                        "        Ok(unsafe {{ crate::Owned::wrap_unchecked(self.0.marshal_array_typed({n}, &mut [{marshal_args}])?) }})"
                    );
                } else {
                    let _ = writeln!(
                        request_wrappers,
                        "        self.0.marshal_array_void({n}, &mut [{marshal_args}])"
                    );
                }
                let _ = writeln!(request_wrappers, "    }}\n");
            }
        }

        let mut if_event_messages = String::new();
        let mut listener_trait_members = String::new();
        let mut listener_fn_wrappers = String::new();
        let mut listener_fn_table_member_defs = String::new();
        let mut listener_fn_table_construct_members = String::new();
        for r in x.events.iter() {
            let event_name_str = &r.name;
            let event_name = RsIdent::Ref(&r.name);

            let mut type_chars = String::with_capacity(r.args.len());
            let mut if_pointers = String::new();
            if let Some(v) = r.since {
                let _ = write!(type_chars, "{v}");
            }
            let mut listener_trait_args = String::new();
            let mut listener_raw_args = String::new();
            let mut listener_arg_conversions = String::new();
            for a in r.args.iter() {
                let arg_name = RsIdent::Ref(&a.name);

                match a.interface {
                    Some(ref t) if t == &x.name => {
                        let _ = write!(if_pointers, "&{if_static_var_name} as *const _,");
                    }
                    Some(ref t) => {
                        let def_ref = if let Some(s) = t.strip_prefix("wl_") {
                            // special case for base interfaces
                            RsPath::CRATE.descendant(RsIdent::Generated(if_name_to_type_name(s)))
                        } else {
                            RsPath::CRATE.descendant(RsIdent::Generated(if_name_to_type_name(t)))
                        };
                        let def_ref = RsPath {
                            parent: Some(Box::new(def_ref)),
                            element: RsPathElement::Ident(RsIdent::Const("DEF")),
                        };

                        let _ = write!(if_pointers, "{def_ref},");
                    }
                    None => if_pointers.push_str("core::ptr::null(),"),
                }

                if a.allow_null {
                    type_chars.push('?');
                }

                type_chars.push(a.r#type.signature_char());

                let _ = write!(
                    listener_raw_args,
                    "{arg_name}: {},",
                    a.r#type.raw_arg_type()
                );
                match (
                    a.interface.as_deref(),
                    a.r#enum.as_deref(),
                    a.r#type,
                    a.allow_null,
                ) {
                    (None, None, WlWireFormatType::UInt, false) => {
                        let _ = write!(listener_trait_args, "{arg_name}: u32,");
                        let _ = write!(listener_arg_conversions, "{arg_name},");
                    }
                    (None, Some(t), WlWireFormatType::UInt, false) => {
                        let _ = write!(
                            listener_trait_args,
                            "{arg_name}: {type_name}{},",
                            enum_type_name(t)
                        );
                        let _ = write!(
                            listener_arg_conversions,
                            "unsafe {{ core::mem::transmute({arg_name}) }},"
                        );
                    }
                    (None, None, WlWireFormatType::Int, false) => {
                        let _ = write!(listener_trait_args, "{arg_name}: i32,");
                        let _ = write!(listener_arg_conversions, "{arg_name},");
                    }
                    (None, Some(t), WlWireFormatType::Int, false) => {
                        let _ = write!(
                            listener_trait_args,
                            "{arg_name}: {type_name}{},",
                            enum_type_name(t)
                        );
                        let _ = write!(
                            listener_arg_conversions,
                            "unsafe {{ core::mem::transmute({arg_name}) }},"
                        );
                    }
                    (Some(o), None, WlWireFormatType::Object, false) => {
                        let _ = write!(
                            listener_trait_args,
                            "{arg_name}: &mut {},",
                            if_name_to_typeref(o)
                        );
                        let _ = write!(
                            listener_arg_conversions,
                            "unsafe {{ &mut *({arg_name} as *mut _) }},"
                        );
                    }
                    (Some(o), None, WlWireFormatType::Object, true) => {
                        let _ = write!(
                            listener_trait_args,
                            "{arg_name}: Option<&mut {}>,",
                            if_name_to_typeref(o)
                        );
                        let _ = write!(
                            listener_arg_conversions,
                            "if {arg_name}.is_null() {{ None }} else {{ Some(unsafe {{ &mut *({arg_name} as *mut _) }}) }},",
                        );
                    }
                    (Some(o), None, WlWireFormatType::NewID, false) => {
                        let _ = write!(
                            listener_trait_args,
                            "{arg_name}: crate::Owned<{}>,",
                            if_name_to_typeref(o)
                        );
                        let _ = write!(
                            listener_arg_conversions,
                            "unsafe {{ crate::Owned::from_untyped_unchecked(core::ptr::NonNull::new_unchecked(crate::Proxy::cast_ffi_ptr({arg_name}))) }},"
                        );
                    }
                    (None, None, WlWireFormatType::Array, false) => {
                        let _ = write!(listener_trait_args, "{arg_name}: &mut ffi::Array,");
                        let _ = write!(listener_arg_conversions, "unsafe {{ &mut *{arg_name} }},");
                    }
                    (None, None, WlWireFormatType::String, false) => {
                        let _ = write!(listener_trait_args, "{arg_name}: &core::ffi::CStr,");
                        let _ = write!(
                            listener_arg_conversions,
                            "unsafe {{ core::ffi::CStr::from_ptr({arg_name}) }},"
                        );
                    }
                    (None, None, WlWireFormatType::Fixed, false) => {
                        let _ = write!(listener_trait_args, "{arg_name}: crate::Fixed,");
                        let _ = write!(listener_arg_conversions, "{arg_name},");
                    }
                    (_, _, WlWireFormatType::UInt, true)
                    | (Some(_), _, WlWireFormatType::UInt, _)
                    | (_, _, WlWireFormatType::Int, true)
                    | (Some(_), _, WlWireFormatType::Int, _)
                    | (_, _, WlWireFormatType::Fixed, true)
                    | (Some(_), _, WlWireFormatType::Fixed, _)
                    | (_, Some(_), WlWireFormatType::Fixed, _)
                    | (_, _, WlWireFormatType::FD, true)
                    | (Some(_), _, WlWireFormatType::FD, _)
                    | (_, Some(_), WlWireFormatType::FD, _)
                    | (None, _, WlWireFormatType::NewID, _)
                    | (Some(_), _, WlWireFormatType::String, _)
                    | (_, Some(_), WlWireFormatType::String, _)
                    | (_, Some(_), WlWireFormatType::Object, _) => {
                        unreachable!(
                            "invalid pattern: {:?} {:?} {:?} {}",
                            a.interface, a.r#enum, a.r#type, a.allow_null
                        )
                    }
                    _ => panic!(
                        "unhandled combination: {:?} {:?} {:?} {}",
                        a.interface, a.r#enum, a.r#type, a.allow_null
                    ),
                }
            }

            let _ = write!(
                if_event_messages,
                r#"ffi::Message {{ name: c"{event_name_str}".as_ptr(), signature: c"{type_chars}".as_ptr(), types: const {{ [{if_pointers}] }}.as_ptr() }},"#,
            );

            let _ = writeln!(
                listener_fn_wrappers,
                r#"extern "C" fn {event_name}<L: {event_listener_trait_name}>(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy,{listener_raw_args}) {{ L::{event_name}(unsafe {{ &mut *(data0 as *mut _) }}, unsafe {{ &mut *(sender0 as *mut _) }},{listener_arg_conversions}) }}"#
            );
            let _ = writeln!(
                listener_fn_table_member_defs,
                r#"{event_name}: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, {listener_raw_args}),"#
            );
            let _ = writeln!(
                listener_fn_table_construct_members,
                "{event_name}: {event_name}::<L>,"
            );
            let _ = writeln!(
                listener_trait_members,
                "    fn {event_name}(&mut self, sender: &mut {type_name}, {listener_trait_args});",
            );
        }

        println!(
            r#"static {if_static_var_name}: ffi::Interface = ffi::Interface {{ name: c"{}".as_ptr(), version: {}, method_count: {method_count}, methods: const {{ [{if_request_messages}] }}.as_ptr(), event_count: {event_count}, events: const {{ [{if_event_messages}] }}.as_ptr() }};"#,
            x.name,
            x.version,
            method_count = x.requests.len(),
            event_count = x.events.len(),
        );
        println!("");
        println!("#[repr(transparent)] pub struct {type_name}(pub(crate) Proxy);");
        println!("unsafe impl Interface for {type_name} {{");
        println!("    const DEF: *const ffi::Interface = &{if_static_var_name};");
        if let Some((opcode, use_simple_call, since)) = destructor {
            println!("");
            println!(
                r#"    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<{type_name} as Interface>::destruct", skip(self)))]"#
            );
            println!("    unsafe fn destruct(&mut self) {{");
            if let Some(v) = since {
                println!("        if self.0.version() < {v} {{ return; }}\n");
            }
            if use_simple_call {
                println!("        self.0.call_simple_dtor({opcode});");
            } else {
                todo!("non simple call");
            }
            println!("    }}");
        }
        println!("}}");
        println!("");
        println!("impl {type_name} {{");
        if !listener_fn_table_member_defs.is_empty() {
            println!(
                "    pub fn set_listener<'l, L: {event_listener_trait_name} + 'l>(&'l mut self, listener: &'l mut L) -> crate::SetListenerResult {{"
            );
            println!("        {listener_fn_wrappers}");
            println!("        #[repr(C)] struct FPTable {{ {listener_fn_table_member_defs} }}");
            println!(
                "        unsafe {{ self.0.set_listener(&const {{ FPTable {{ {listener_fn_table_construct_members} }} }} as &'static FPTable as *const _ as _,listener as *mut _ as _) }}"
            );
            println!("    }}");
            println!("");
        }
        println!("{request_wrappers} }}\n");
        if !listener_trait_members.is_empty() {
            println!("pub trait {event_listener_trait_name} {{ {listener_trait_members} }}\n");
        }

        for e in x.enums.iter() {
            let enum_name = enum_type_name(&e.name);

            if e.bitfield
                .as_ref()
                .is_some_and(|x| x.eq_ignore_ascii_case("true"))
            {
                // bitfield
                println!(
                    "bitflags::bitflags! {{ #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub struct {type_name}{enum_name} : u32 {{",
                );
                for ee in e.entries.iter() {
                    println!("    const {} = {};", enum_entry_name(&ee.name), ee.value);
                }
                println!("}} }}");
                println!(
                    "impl {type_name}{enum_name} {{ pub const fn as_arg(&self) -> ffi::Argument {{ ffi::Argument {{ u: self.bits() }} }} }}\n"
                );
            } else {
                println!(
                    "#[repr(u32)] #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub enum {type_name}{enum_name} {{",
                );
                for ee in e.entries.iter() {
                    println!("    {} = {},", enum_entry_name(&ee.name), ee.value);
                }
                println!("}}");
                println!(
                    "impl {type_name}{enum_name} {{ pub const fn as_arg(&self) -> ffi::Argument {{ ffi::Argument {{ u: *self as _ }} }} }}\n"
                );
            }
        }
    }

    Ok(())
}

fn xml_document(reader: &mut Reader<&[u8]>) -> Result<XmlProtocol, quick_xml::Error> {
    let mut proto = None;
    loop {
        match reader.read_event()? {
            Event::Eof => break,
            Event::Text(_) => (/* ignore */),
            Event::Decl(_) => (/* ignore */),
            Event::Start(t) if t.name().0 == b"protocol" => {
                if proto.is_some() {
                    panic!("multiple root");
                }

                proto = Some(XmlProtocol::read(t, reader)?);
            }
            e => panic!("unexpected: {e:?}"),
        }
    }

    Ok(proto.expect("least one protocol required"))
}

#[derive(Debug)]
pub struct XmlProtocol {
    pub name: String,
    pub copyright: Option<String>,
    pub description: Option<XmlDescription>,
    pub interfaces: Vec<XmlInterface>,
}
impl XmlProtocol {
    pub fn read<'a>(
        tag: BytesStart<'a>,
        reader: &mut Reader<&'a [u8]>,
    ) -> Result<Self, quick_xml::Error> {
        let mut name = None;
        for a in tag.attributes() {
            let a = a?;
            if a.key.0 == b"name" {
                name = Some(a.decode_and_unescape_value(reader.decoder())?.into_owned());
            }
        }

        let mut copyright = None;
        let mut description = None;
        let mut interfaces = Vec::new();
        loop {
            match reader.read_event()? {
                Event::End(e) if e.name().0 == b"protocol" => break,
                Event::Start(t) if t.name().0 == b"copyright" => {
                    copyright = Some(xml_copyright(reader)?);
                }
                Event::Start(t) if t.name().0 == b"description" => {
                    description = Some(XmlDescription::read(t, reader)?);
                }
                Event::Start(t) if t.name().0 == b"interface" => {
                    interfaces.push(XmlInterface::read(t, reader)?);
                }
                Event::Text(_) => (/* ignore */),
                e => panic!("unexpected: {e:?}"),
            }
        }

        Ok(Self {
            name: name.expect("required"),
            copyright,
            description,
            interfaces,
        })
    }
}

fn xml_copyright<'a>(reader: &mut Reader<&'a [u8]>) -> Result<String, quick_xml::Error> {
    let mut content = String::new();
    loop {
        match reader.read_event()? {
            Event::End(e) if e.name().0 == b"copyright" => break Ok(content),
            Event::Text(t) => {
                content.push_str(&t.decode()?);
            }
            Event::Comment(_) => (/* ignore */),
            e => panic!("unexpected: {e:?}"),
        }
    }
}

#[derive(Debug)]
pub struct XmlInterface {
    pub name: String,
    pub version: usize,
    pub description: Option<XmlDescription>,
    pub requests: Vec<XmlRequest>,
    pub events: Vec<XmlEvent>,
    pub enums: Vec<XmlEnum>,
}
impl XmlInterface {
    pub fn read(tag: BytesStart, reader: &mut Reader<&[u8]>) -> Result<Self, quick_xml::Error> {
        let mut name = None;
        let mut version = None;
        for a in tag.attributes() {
            let a = a?;
            if a.key.0 == b"name" {
                name = Some(a.decode_and_unescape_value(reader.decoder())?.into_owned());
            } else if a.key.0 == b"version" {
                version = Some(
                    a.decode_and_unescape_value(reader.decoder())?
                        .parse()
                        .expect("invalid version number"),
                );
            }
        }

        let mut description = None;
        let mut requests = Vec::new();
        let mut events = Vec::new();
        let mut enums = Vec::new();
        loop {
            match reader.read_event()? {
                Event::End(e) if e.name().0 == b"interface" => break,
                Event::Start(t) if t.name().0 == b"description" => {
                    description = Some(XmlDescription::read(t, reader)?);
                }
                Event::Start(t) if t.name().0 == b"request" => {
                    requests.push(XmlRequest::read(t, reader)?);
                }
                Event::Start(t) if t.name().0 == b"event" => {
                    events.push(XmlEvent::read(t, reader)?);
                }
                Event::Start(t) if t.name().0 == b"enum" => {
                    enums.push(XmlEnum::read(t, reader)?);
                }
                Event::Text(_) => (/* ignore */),
                Event::Comment(_) => (/* ignore */),
                e => panic!("unexpected: {e:?}"),
            }
        }

        Ok(Self {
            name: name.expect("required"),
            version: version.expect("required"),
            description,
            requests,
            events,
            enums,
        })
    }
}

#[derive(Debug)]
pub struct XmlRequest {
    pub name: String,
    pub r#type: Option<String>,
    pub since: Option<usize>,
    pub deprecated_since: Option<usize>,
    pub description: Option<XmlDescription>,
    pub args: Vec<XmlArg>,
}
impl XmlRequest {
    pub fn read<'a>(
        tag: BytesStart<'a>,
        reader: &mut Reader<&'a [u8]>,
    ) -> Result<Self, quick_xml::Error> {
        let mut name = None;
        let mut r#type = None;
        let mut since = None;
        let mut deprecated_since = None;
        for a in tag.attributes() {
            let a = a?;
            if a.key.0 == b"name" {
                name = Some(a.decode_and_unescape_value(reader.decoder())?.into_owned());
            } else if a.key.0 == b"type" {
                r#type = Some(a.decode_and_unescape_value(reader.decoder())?.into_owned());
            } else if a.key.0 == b"since" {
                since = Some(
                    a.decode_and_unescape_value(reader.decoder())?
                        .parse()
                        .expect("invalid version number"),
                );
            } else if a.key.0 == b"deprecated-since" {
                deprecated_since = Some(
                    a.decode_and_unescape_value(reader.decoder())?
                        .parse()
                        .expect("invalid version number"),
                );
            }
        }

        let mut description = None;
        let mut args = Vec::new();
        loop {
            match reader.read_event()? {
                Event::End(e) if e.name().0 == b"request" => break,
                Event::Start(t) if t.name().0 == b"description" => {
                    description = Some(XmlDescription::read(t, reader)?);
                }
                Event::Start(t) if t.name().0 == b"arg" => {
                    args.push(XmlArg::read(t, false, reader)?);
                }
                Event::Empty(t) if t.name().0 == b"arg" => {
                    args.push(XmlArg::read(t, true, reader)?);
                }
                Event::Text(_) => (/* ignore */),
                e => panic!("unexpected: {e:?}"),
            }
        }

        Ok(XmlRequest {
            name: name.expect("required"),
            r#type,
            since,
            deprecated_since,
            description,
            args,
        })
    }
}

#[derive(Debug)]
pub struct XmlEvent {
    pub name: String,
    pub r#type: Option<String>,
    pub since: Option<usize>,
    pub deprecated_since: Option<usize>,
    pub description: Option<XmlDescription>,
    pub args: Vec<XmlArg>,
}
impl XmlEvent {
    pub fn read<'a>(
        tag: BytesStart<'a>,
        reader: &mut Reader<&'a [u8]>,
    ) -> Result<Self, quick_xml::Error> {
        let mut name = None;
        let mut r#type = None;
        let mut since = None;
        let mut deprecated_since = None;
        for a in tag.attributes() {
            let a = a?;
            if a.key.0 == b"name" {
                name = Some(a.decode_and_unescape_value(reader.decoder())?.into_owned());
            } else if a.key.0 == b"type" {
                r#type = Some(a.decode_and_unescape_value(reader.decoder())?.into_owned());
            } else if a.key.0 == b"since" {
                since = Some(
                    a.decode_and_unescape_value(reader.decoder())?
                        .parse()
                        .expect("invalid version number"),
                );
            } else if a.key.0 == b"deprecated-since" {
                deprecated_since = Some(
                    a.decode_and_unescape_value(reader.decoder())?
                        .parse()
                        .expect("invalid version number"),
                );
            }
        }

        let mut description = None;
        let mut args = Vec::new();
        loop {
            match reader.read_event()? {
                Event::End(e) if e.name().0 == b"event" => break,
                Event::Start(t) if t.name().0 == b"description" => {
                    description = Some(XmlDescription::read(t, reader)?);
                }
                Event::Start(t) if t.name().0 == b"arg" => {
                    args.push(XmlArg::read(t, false, reader)?);
                }
                Event::Empty(t) if t.name().0 == b"arg" => {
                    args.push(XmlArg::read(t, true, reader)?);
                }
                Event::Text(_) => (/* ignore */),
                e => panic!("unexpected: {e:?}"),
            }
        }

        Ok(XmlEvent {
            name: name.expect("required"),
            r#type,
            since,
            deprecated_since,
            description,
            args,
        })
    }
}

#[derive(Debug)]
pub struct XmlEnum {
    pub name: String,
    pub since: Option<usize>,
    pub bitfield: Option<String>,
    pub description: Option<XmlDescription>,
    pub entries: Vec<XmlEntry>,
}
impl XmlEnum {
    pub fn read<'a>(
        tag: BytesStart<'a>,
        reader: &mut Reader<&'a [u8]>,
    ) -> Result<Self, quick_xml::Error> {
        let mut name = None;
        let mut since = None;
        let mut bitfield = None;
        for a in tag.attributes() {
            let a = a?;
            if a.key.0 == b"name" {
                name = Some(a.decode_and_unescape_value(reader.decoder())?.into_owned());
            } else if a.key.0 == b"since" {
                since = Some(
                    a.decode_and_unescape_value(reader.decoder())?
                        .parse()
                        .expect("invalid version number"),
                );
            } else if a.key.0 == b"bitfield" {
                bitfield = Some(a.decode_and_unescape_value(reader.decoder())?.into_owned());
            }
        }

        let mut description = None;
        let mut entries = Vec::new();
        loop {
            match reader.read_event()? {
                Event::End(e) if e.name().0 == b"enum" => break,
                Event::Start(t) if t.name().0 == b"description" => {
                    description = Some(XmlDescription::read(t, reader)?);
                }
                Event::Start(t) if t.name().0 == b"entry" => {
                    entries.push(XmlEntry::read(t, false, reader)?);
                }
                Event::Empty(t) if t.name().0 == b"entry" => {
                    entries.push(XmlEntry::read(t, true, reader)?);
                }
                Event::Text(_) => (/* ignore */),
                e => panic!("unexpected: {e:?}"),
            }
        }

        Ok(XmlEnum {
            name: name.expect("required"),
            since,
            bitfield,
            description,
            entries,
        })
    }
}

#[derive(Debug)]
pub struct XmlEntry {
    pub name: String,
    pub value: usize,
    pub summary: Option<String>,
    pub since: Option<usize>,
    pub deprecated_since: Option<usize>,
    pub description: Option<XmlDescription>,
}
impl XmlEntry {
    pub fn read<'a>(
        tag: BytesStart<'a>,
        empty: bool,
        reader: &mut Reader<&'a [u8]>,
    ) -> Result<Self, quick_xml::Error> {
        let mut name = None;
        let mut value = None;
        let mut summary = None;
        let mut since = None;
        let mut deprecated_since = None;
        for a in tag.attributes() {
            let a = a?;
            if a.key.0 == b"name" {
                name = Some(a.decode_and_unescape_value(reader.decoder())?.into_owned());
            } else if a.key.0 == b"value" {
                let s = a.decode_and_unescape_value(reader.decoder())?;
                if let Some(h) = s.strip_prefix("0x") {
                    // hex-number
                    value = Some(usize::from_str_radix(h, 16).expect("invalid enum value"));
                } else {
                    value = Some(s.parse().expect("invalid enum value"));
                }
            } else if a.key.0 == b"summary" {
                summary = Some(a.decode_and_unescape_value(reader.decoder())?.into_owned());
            } else if a.key.0 == b"since" {
                since = Some(
                    a.decode_and_unescape_value(reader.decoder())?
                        .parse()
                        .expect("invalid version number"),
                );
            } else if a.key.0 == b"deprecated-since" {
                deprecated_since = Some(
                    a.decode_and_unescape_value(reader.decoder())?
                        .parse()
                        .expect("invalid version number"),
                );
            }
        }

        let mut description = None;
        if !empty {
            loop {
                match reader.read_event()? {
                    Event::End(e) if e.name().0 == b"entry" => break,
                    Event::Start(t) if t.name().0 == b"description" => {
                        description = Some(XmlDescription::read(t, reader)?);
                    }
                    Event::Text(_) => (/* ignore */),
                    e => panic!("unexpected: {e:?}"),
                }
            }
        }

        Ok(XmlEntry {
            name: name.expect("required"),
            value: value.expect("required"),
            summary,
            since,
            deprecated_since,
            description,
        })
    }
}

#[derive(Debug)]
pub struct XmlArg {
    pub name: String,
    pub r#type: WlWireFormatType,
    pub summary: Option<String>,
    pub interface: Option<String>,
    pub allow_null: bool,
    pub r#enum: Option<String>,
    pub description: Option<XmlDescription>,
}
impl XmlArg {
    pub fn read<'a>(
        tag: BytesStart<'a>,
        empty: bool,
        reader: &mut Reader<&'a [u8]>,
    ) -> Result<Self, quick_xml::Error> {
        let mut name = None;
        let mut r#type = None;
        let mut summary = None;
        let mut interface = None;
        let mut allow_null = false;
        let mut r#enum = None;
        for a in tag.attributes() {
            let a = a?;
            if a.key.0 == b"name" {
                name = Some(a.decode_and_unescape_value(reader.decoder())?.into_owned());
            } else if a.key.0 == b"type" {
                r#type = Some(
                    WlWireFormatType::try_from_xml_str(
                        &a.decode_and_unescape_value(reader.decoder())?,
                    )
                    .expect("invalid arg type"),
                );
            } else if a.key.0 == b"summary" {
                summary = Some(a.decode_and_unescape_value(reader.decoder())?.into_owned());
            } else if a.key.0 == b"interface" {
                interface = Some(a.decode_and_unescape_value(reader.decoder())?.into_owned());
            } else if a.key.0 == b"allow-null" {
                allow_null = a.as_bool().expect("invalid allow-null value");
            } else if a.key.0 == b"enum" {
                r#enum = Some(a.decode_and_unescape_value(reader.decoder())?.into_owned());
            }
        }

        let mut description = None;
        if !empty {
            loop {
                match reader.read_event()? {
                    Event::End(e) if e.name().0 == b"arg" => break,
                    Event::Start(t) if t.name().0 == b"description" => {
                        description = Some(XmlDescription::read(t, reader)?);
                    }
                    Event::Text(_) => (/* ignore */),
                    e => panic!("unexpected: {e:?}"),
                }
            }
        }

        Ok(XmlArg {
            name: name.expect("required"),
            r#type: r#type.expect("required"),
            summary,
            interface,
            allow_null,
            r#enum,
            description,
        })
    }
}

#[derive(Debug)]
pub struct XmlDescription {
    pub summary: String,
    pub content: String,
}
impl XmlDescription {
    pub fn read<'a>(
        tag: BytesStart<'a>,
        reader: &mut Reader<&'a [u8]>,
    ) -> Result<Self, quick_xml::Error> {
        let mut summary = None;
        for a in tag.attributes() {
            let a = a?;
            if a.key.0 == b"summary" {
                summary = Some(a.decode_and_unescape_value(reader.decoder())?.into_owned());
            }
        }

        let mut content = String::new();
        loop {
            match reader.read_event()? {
                Event::End(e) if e.name().0 == b"description" => break,
                Event::Text(t) => {
                    content.push_str(&t.decode()?);
                }
                Event::Comment(_) => (/* ignore */),
                e => panic!("unexpected: {e:?}"),
            }
        }

        Ok(XmlDescription {
            summary: summary.expect("required"),
            content,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WlWireFormatType {
    Int,
    UInt,
    Fixed,
    String,
    Object,
    NewID,
    Array,
    FD,
}
impl WlWireFormatType {
    pub fn try_from_xml_str<'s>(x: &'s str) -> Result<Self, &'s str> {
        match x {
            "int" => Ok(Self::Int),
            "uint" => Ok(Self::UInt),
            "fixed" => Ok(Self::Fixed),
            "string" => Ok(Self::String),
            "object" => Ok(Self::Object),
            "new_id" => Ok(Self::NewID),
            "array" => Ok(Self::Array),
            "fd" => Ok(Self::FD),
            x => Err(x),
        }
    }

    pub const fn signature_char(&self) -> char {
        match self {
            Self::Int => 'i',
            Self::UInt => 'u',
            Self::Fixed => 'f',
            Self::String => 's',
            Self::Object => 'o',
            Self::NewID => 'n',
            Self::Array => 'a',
            Self::FD => 'h',
        }
    }

    pub const fn raw_arg_type(&self) -> &'static str {
        match self {
            Self::Int => "i32",
            Self::UInt => "u32",
            Self::Fixed => "ffi::Fixed",
            Self::String => "*const core::ffi::c_char",
            Self::Object => "*mut ffi::Proxy",
            Self::NewID => "*mut ffi::Proxy",
            Self::Array => "*mut ffi::Array",
            Self::FD => "i32",
        }
    }
}
