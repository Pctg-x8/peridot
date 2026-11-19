use proc_macro::{Span, TokenStream};
use quote::ToTokens;
use syn::{
    Block, BlockModifiers, Expr, ExprCall, ExprConst, ExprLit, ExprMethodCall, ExprPath,
    ExprReference, Ident, ItemFn, Lifetime, Lit, LitStr, Local, LocalInit, LocalModifiers, Pat,
    PatIdent, PatType, Path, PathArguments, PathSegment, Stmt, Type, TypePath, TypeReference,
    parse::Parse,
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    token::{And, Brace, Colon, Comma, Const, Dot, Eq, Let, Paren, Semi},
};

struct InstrumentArgs {
    name: Option<LitStr>,
}
impl Parse for InstrumentArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut args = Self { name: None };
        while !input.is_empty() {
            let la = input.lookahead1();
            if la.peek(Comma) {
                input.parse::<Comma>()?;
            } else {
                if args.name.is_some() {
                    return Err(input.error("multiple name specified"));
                }
                args.name = Some(input.parse::<LitStr>()?);
            }
        }
        Ok(args)
    }
}

#[proc_macro_attribute]
pub fn instrument(args: TokenStream, item: TokenStream) -> TokenStream {
    if !cfg!(feature = "active") {
        // activeじゃなければitemそのまま
        return item;
    }

    let mut input = parse_macro_input!(item as ItemFn);
    let mut args = parse_macro_input!(args as InstrumentArgs);

    let marker_ident = Ident::new(
        "__profiler_inline_instrument_marker",
        Span::call_site().into(),
    );
    let profiler_section_type = Type::Path(TypePath {
        attrs: Vec::new(),
        qself: None,
        path: Path {
            leading_colon: None,
            segments: Punctuated::from_iter([
                PathSegment {
                    ident: Ident::new("profiler", Span::call_site().into()),
                    arguments: PathArguments::None,
                },
                PathSegment {
                    ident: Ident::new("Section", Span::call_site().into()),
                    arguments: PathArguments::None,
                },
            ]),
        },
    });
    input.block.stmts = [
        Stmt::Local(Local {
            attrs: Vec::new(),
            let_token: Let::default(),
            modifiers: LocalModifiers::default(),
            pat: Pat::Type(PatType {
                attrs: Vec::new(),
                pat: Box::new(Pat::Ident(PatIdent {
                    attrs: Vec::new(),
                    by_ref: None,
                    mutability: None,
                    ident: marker_ident.clone(),
                    subpat: None,
                })),
                colon_token: Colon::default(),
                ty: Box::new(Type::Reference(TypeReference {
                    attrs: Vec::new(),
                    and_token: And::default(),
                    lifetime: Some(Lifetime::new("'static", Span::call_site().into())),
                    mutability: None,
                    elem: Box::new(profiler_section_type),
                })),
            }),
            init: Some(LocalInit {
                eq_token: Eq::default(),
                expr: Box::new(Expr::Reference(ExprReference {
                    attrs: Vec::new(),
                    and_token: And::default(),
                    mutability: None,
                    expr: Box::new(Expr::Const(ExprConst {
                        attrs: Vec::new(),
                        const_token: Const::default(),
                        modifiers: BlockModifiers::default(),
                        block: Block {
                            brace_token: Brace::default(),
                            stmts: vec![Stmt::Expr(
                                Expr::Call(ExprCall {
                                    attrs: Vec::new(),
                                    func: Box::new(Expr::Path(ExprPath {
                                        attrs: Vec::new(),
                                        qself: None,
                                        path: Path {
                                            leading_colon: None,
                                            segments: Punctuated::from_iter([
                                                PathSegment {
                                                    ident: Ident::new("profiler", input.span()),
                                                    arguments: PathArguments::None,
                                                },
                                                PathSegment {
                                                    ident: Ident::new("Section", input.span()),
                                                    arguments: PathArguments::None,
                                                },
                                                PathSegment {
                                                    ident: Ident::new("new", input.span()),
                                                    arguments: PathArguments::None,
                                                },
                                            ]),
                                        },
                                    })),
                                    paren_token: Paren::default(),
                                    args: Punctuated::from_iter([Expr::Lit(ExprLit {
                                        attrs: Vec::new(),
                                        lit: Lit::Str(args.name.take().unwrap_or_else(|| {
                                            LitStr::new(&input.sig.ident.to_string(), input.span())
                                        })),
                                    })]),
                                }),
                                None,
                            )],
                        },
                    })),
                })),
                diverge: None,
            }),
            semi_token: Semi::default(),
        }),
        Stmt::Local(Local {
            attrs: Vec::new(),
            let_token: Let::default(),
            modifiers: LocalModifiers::default(),
            pat: Pat::Ident(PatIdent {
                attrs: Vec::new(),
                by_ref: None,
                mutability: None,
                ident: Ident::new("__profiler_instrument", Span::call_site().into()),
                subpat: None,
            }),
            init: Some(LocalInit {
                eq_token: Eq::default(),
                expr: Box::new(Expr::Call(ExprCall {
                    attrs: Vec::new(),
                    func: Box::new(Expr::Path(ExprPath {
                        attrs: Vec::new(),
                        qself: None,
                        path: Path {
                            leading_colon: None,
                            segments: Punctuated::from_iter([
                                PathSegment {
                                    ident: Ident::new("profiler", Span::call_site().into()),
                                    arguments: PathArguments::None,
                                },
                                PathSegment {
                                    ident: Ident::new("SectionScope", Span::call_site().into()),
                                    arguments: PathArguments::None,
                                },
                            ]),
                        },
                    })),
                    paren_token: Paren::default(),
                    args: Punctuated::from_iter([Expr::MethodCall(ExprMethodCall {
                        attrs: Vec::new(),
                        receiver: Box::new(Expr::Call(ExprCall {
                            attrs: Vec::new(),
                            func: Box::new(Expr::Path(ExprPath {
                                attrs: Vec::new(),
                                qself: None,
                                path: Path {
                                    leading_colon: None,
                                    segments: Punctuated::from_iter([
                                        PathSegment {
                                            ident: Ident::new("profiler", Span::call_site().into()),
                                            arguments: PathArguments::None,
                                        },
                                        PathSegment {
                                            ident: Ident::new("profiler", Span::call_site().into()),
                                            arguments: PathArguments::None,
                                        },
                                    ]),
                                },
                            })),
                            paren_token: Paren::default(),
                            args: Punctuated::new(),
                        })),
                        dot_token: Dot::default(),
                        method: Ident::new("emit_section_begin", Span::call_site().into()),
                        turbofish: None,
                        paren_token: Paren::default(),
                        args: Punctuated::from_iter([Expr::Path(ExprPath {
                            attrs: Vec::new(),
                            qself: None,
                            path: Path {
                                leading_colon: None,
                                segments: Punctuated::from_iter([PathSegment {
                                    ident: marker_ident,
                                    arguments: PathArguments::None,
                                }]),
                            },
                        })]),
                    })]),
                })),
                diverge: None,
            }),
            semi_token: Semi::default(),
        }),
    ]
    .into_iter()
    .chain(input.block.stmts)
    .collect();

    input.into_token_stream().into()
}
