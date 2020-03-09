// extern crate proc_macro;
extern crate proc_macro2;

extern crate quote;
extern crate syn;

use proc_macro::TokenStream;

use quote::quote;
use std::iter::FromIterator;

use syn::parse::{Parse, ParseStream};
use syn::{braced, parenthesized, parse_macro_input, Result, Token};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as SeqMacroInput);

    // let ident = input.ident;
    let starti = input.starti;
    let endi = input.endi;
    // let fnbody = input.fnbody;
    let enumbody = input.enumbody;

    let a = starti.base10_parse::<u64>().unwrap();
    let b = endi.base10_parse::<u64>().unwrap();

    if enumbody.expand_section.is_some() {
        // exps : expands
        let exps: Vec<_> = (a..b)
            .into_iter()
            .map(|n| expand_enum(&enumbody, n))
            .collect();
        let dm = if let Some(mac) = enumbody.attrs {
            let punct = mac.punct.clone();
            let group = mac.group.clone();
            quote! {
                #punct#group
            }
        } else {
            quote! {}
        };
        // eprintln!("????{:#?}", dm);

        let eident = enumbody.ename.clone();
        // eprintln!("????{:#?}", eident);

        let ret = quote! {
            #dm
            enum #eident{
                #(#exps)*
            }
        };

        // eprintln!("????{:#?}", ret);
        ret.into()
    } else {
        unimplemented!()
    }
    // proc_macro::TokenStream::new()
}

#[derive(Debug)]
struct SeqMacroInput {
    /* ... */
    ident: syn::Ident,
    // intok: Token![in], // we know from context
    starti: syn::LitInt,
    // dottok: Token![..],
    endi: syn::LitInt,
    // block: syn::token::Brace,
    // fnbody: FnContent,
    enumbody: EnumContent,
}

#[derive(Debug)]
struct FnContent {
    fname: proc_macro2::Ident,
    replace: proc_macro2::Ident,
    ret: proc_macro2::Ident,
    replace2: proc_macro2::Ident,
    op: Token![*],
    lit: syn::LitInt,
}

#[derive(Debug, Clone)]
struct DeriveMacro {
    punct: proc_macro2::Punct,
    group: proc_macro2::Group,
}

#[derive(Debug, Clone)]
struct EnumContent {
    attrs: Option<DeriveMacro>,
    ename: proc_macro2::Ident,
    expand_section: Option<ReplaceSection>,
}

#[derive(Debug, Clone)]
struct ReplaceSection {
    ident: proc_macro2::Ident,
    repl: proc_macro2::Ident,
    punct: proc_macro2::Punct,
}

impl Parse for DeriveMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            punct: input.parse()?,
            group: input.parse()?,
        })
    }
}

impl Parse for EnumContent {
    fn parse(input: ParseStream) -> Result<Self> {
        // eprintln!(">>>>>{:#?}", input);
        let attrs = if input.peek(Token![#]) && input.peek2(syn::token::Bracket) {
            // parse the derive macro if exists
            let res = input.call(DeriveMacro::parse)?;

            Some(res)
        } else {
            None
        };
        // parse the enum struct

        let _x: Token![enum] = input.parse()?;

        let ename: proc_macro2::Ident = input.parse()?;
        let body;

        let _: syn::token::Brace = braced!(body in input);

        let expand_section = if body.peek(Token![#]) && body.peek2(syn::token::Paren) {
            let _: proc_macro2::Punct = body.parse()?; //# token
            let content;
            let _: syn::token::Paren = parenthesized!(content in body);
            let repl = content.call(ReplaceSection::parse)?;
            let _: proc_macro2::Punct = body.parse()?; //* token
            Some(repl)
        } else {
            None
        };

        Ok(Self {
            attrs,
            ename,
            expand_section,
        })
    }
}

impl Parse for ReplaceSection {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: proc_macro2::Ident = input.parse()?;
        let _: proc_macro2::Punct = input.parse()?;
        let repl: proc_macro2::Ident = input.parse()?;
        let punct: proc_macro2::Punct = input.parse()?;

        Ok(Self { ident, repl, punct })
    }
}

impl Parse for FnContent {
    fn parse(content: ParseStream) -> Result<Self> {
        let _: Token![fn] = content.parse()?; //fn
        let fname: proc_macro2::Ident = content.parse()?; //f
        let _: proc_macro2::Punct = content.parse()?; //#
        let replace: proc_macro2::Ident = content.parse()?; //N
        let _: proc_macro2::Group = content.parse()?; //()
        let _: proc_macro2::Punct = content.parse()?; //-
        let _: proc_macro2::Punct = content.parse()?; //>
        let ret: proc_macro2::Ident = content.parse()?; //u64

        let body;
        let _: syn::token::Brace = braced!(body in content); //{}

        // eprintln!("+++{:#?}", content);
        let replace2: proc_macro2::Ident = body.parse()?; //N
        let op: Token![*] = body.parse()?;
        let lit: syn::LitInt = body.parse()?;

        Ok(FnContent {
            fname,
            replace,
            ret,
            replace2,
            op,
            lit,
        })
    }
}

impl Parse for SeqMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: syn::Ident = input.parse()?; //N
        let _: Token![in] = input.parse()?;
        let starti: syn::LitInt = input.parse()?; //1
        let _: Token![..] = input.parse()?; //..
        let endi: syn::LitInt = input.parse()?; //4

        let content;
        let _: syn::token::Brace = braced!(content in input);

        let res = content.call(EnumContent::parse).unwrap();
        // eprintln!("---{:#?}", res);

        let ret = SeqMacroInput {
            ident,
            // intok,
            starti,
            // dottok,
            endi,
            // block,
            enumbody: res,
        };
        Ok(ret)
    }
}

fn expand_enum(input: &EnumContent, n: u64) -> proc_macro2::TokenStream {
    if let Some(esec) = input.expand_section.clone() {
        let id = esec.ident.to_string();
        let new_id = format!("{}{}", id, n);
        let nident = proc_macro2::Ident::new(new_id.as_str(), esec.ident.span());

        quote! {
            #nident,
        }
    } else {
        unimplemented!()
    }
}

// expand the stmt expression
fn expand(input: &FnContent, n: u64) -> proc_macro2::TokenStream {
    let fname = input.fname.to_string();
    let nfname = format!("{}{}", fname, n);
    let ret = input.ret.clone();
    let op = input.op.clone();
    let lit = input.lit.clone();

    let nfname = proc_macro2::Ident::new(nfname.as_str(), input.fname.span());
    let nlit = proc_macro2::Literal::u64_unsuffixed(n);

    let ret = quote! {
        fn #nfname() -> #ret{
            #nlit #op #lit
        }
    };
    ret
}

// expand TokenStream
fn expand_token_stream(
    rident: proc_macro2::Ident,
    n: u64,
    tt: proc_macro2::TokenTree,
) -> proc_macro2::TokenTree {
    match tt {
        proc_macro2::TokenTree::Group(g) => {
            let ns: Vec<proc_macro2::TokenTree> = g
                .stream()
                .into_iter()
                .map(|it| {
                    // expand the nested tokenstream of
                    match it {
                        proc_macro2::TokenTree::Group(gg) => {
                            //expand the target token stream
                            let tts: Vec<proc_macro2::TokenTree> = gg
                                .stream()
                                .into_iter()
                                .map(|item| match item {
                                    proc_macro2::TokenTree::Ident(ident) => {
                                        // change ident N to literal
                                        let mut lit = proc_macro2::Literal::u64_unsuffixed(n);
                                        lit.set_span(ident.span());
                                        proc_macro2::TokenTree::Literal(lit)
                                    }
                                    xx => xx,
                                })
                                .collect();
                            let ng = proc_macro2::Group::new(
                                gg.delimiter(),
                                proc_macro2::TokenStream::from_iter(tts),
                            );
                            proc_macro2::TokenTree::Group(ng)
                        }
                        it => it,
                    }
                })
                .collect();

            let ns_iter = ns.into_iter();
            let ns = proc_macro2::TokenStream::from_iter(ns_iter);
            let ng = proc_macro2::Group::new(g.delimiter(), ns);
            proc_macro2::TokenTree::Group(ng)
        }
        proc_macro2::TokenTree::Ident(ident) if ident.to_string() == rident.to_string() => {
            let mut lit = proc_macro2::Literal::u64_unsuffixed(n);
            lit.set_span(ident.span());
            proc_macro2::TokenTree::Literal(lit)
        }
        t => t,
    }
}
