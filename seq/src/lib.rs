// extern crate proc_macro;
extern crate proc_macro2;

extern crate quote;
extern crate syn;

use proc_macro::TokenStream;

use quote::quote;
use std::iter::FromIterator;

use syn::parse::{Parse, ParseStream};
use syn::{braced, parse_macro_input, Result, Token};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as SeqMacroInput);

    let ident = input.ident;
    let starti = input.starti;
    let endi = input.endi;
    let stream = input.stream;

    let a = starti.base10_parse::<u64>().unwrap();
    let b = endi.base10_parse::<u64>().unwrap();

    let fns = (a..b)
        .into_iter()
        .map(|n| expand(ident.clone(), n, expr.clone()))
        .collect();

    let ret = quote! {
        // #(#stmts)*
    };

    ret.into()
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
    stream: proc_macro2::TokenStream,
}

impl Parse for SeqMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: syn::Ident = input.parse()?;
        let _: Token![in] = input.parse()?;
        let starti: syn::LitInt = input.parse()?;
        let _: Token![..] = input.parse()?;
        let endi: syn::LitInt = input.parse()?;

        let content;
        let mut block = braced!(content in input);
        let stream = content;
        eprintln!("{:#?}", content);

        let a = starti.base10_parse::<u64>()?;
        let b = endi.base10_parse::<u64>()?;

        let ret = SeqMacroInput {
            ident,
            // intok,
            starti,
            // dottok,
            endi,
            // block,
            stream,
        };
        // eprintln!("{:#?}==",ret);

        Ok(ret)
    }
}

// expand the stmt expression
fn expand(ident: syn::Ident, n: u64, expr: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let next_ident = false;
    expr.into_iter()
        .filter_map(|tt| match tt {
            proc_macro2::TokenTree::Ident(ident) if next_ident => {}
            proc_macro2::TokenTree::Punct(p) => {
                if p.as_char() == '#'{
                    
                }
                
                None
            }
            tt => Some(tt),
        })
        .collect()
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
