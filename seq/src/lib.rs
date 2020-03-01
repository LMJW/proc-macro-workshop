// extern crate proc_macro;
extern crate proc_macro2;

extern crate quote;
extern crate syn;

use proc_macro::TokenStream;

use quote::quote;
use std::iter::FromIterator;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Result, Token};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    // let _ = input;
    let input = parse_macro_input!(input as SeqMacroInput);

    let _ident = input.ident;
    let _intok = input.intok;
    let _starti = input.starti;
    let _dottok = input.dottok;
    let _endi = input.endi;
    let stmts = input.block.stmts;

    let ret = quote! {
        #(#stmts)*
    };

    // eprintln!("--{:#?}--",ret);
    ret.into()
    // proc_macro::TokenStream::new()
}

#[derive(Debug)]
struct SeqMacroInput {
    /* ... */
    ident: syn::Ident,
    intok: Token![in],
    starti: syn::LitInt,
    dottok: Token![..],
    endi: syn::LitInt,
    block: syn::Block,
}

impl Into<TokenStream> for SeqMacroInput {
    fn into(self) -> TokenStream {
        unimplemented!()
    }
}

impl Parse for SeqMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: syn::Ident = input.parse()?;
        let intok: Token![in] = input.parse()?;
        let starti: syn::LitInt = input.parse()?;
        let dottok: Token![..] = input.parse()?;
        let endi: syn::LitInt = input.parse()?;

        let mut block: syn::Block = input.parse()?;
        let content = block.stmts.clone();
        // eprintln!("{:#?}", content);

        let a = starti.base10_parse::<u64>()?;
        let b = endi.base10_parse::<u64>()?;

        let mut out = Vec::<syn::Stmt>::new();
        for i in a..b {
            let t: Vec<_> = content
                .clone()
                .into_iter()
                .map(|stmt| expand(ident.clone(), i, stmt))
                .collect();
            out.extend(t);
        }
        block.stmts = out;
        let ret = SeqMacroInput {
            ident,
            intok,
            starti,
            dottok,
            endi,
            block,
        };
        // eprintln!("{:#?}==",ret);

        Ok(ret)
    }
}

// expand the stmt expression
fn expand(ident: syn::Ident, n: u64, expr: syn::Stmt) -> syn::Stmt {
    match expr {
        syn::Stmt::Semi(expr, semi) => {
            let nexp = match expr {
                syn::Expr::Macro(exp) => {
                    let mac = exp.mac;

                    // expand the tokens field of syn::Macro
                    let nts = mac
                        .tokens
                        .into_iter()
                        .map(|tt: proc_macro2::TokenTree| -> proc_macro2::TokenTree {
                            expand_token_stream(ident.clone(), n, tt)
                        })
                        .collect();
                    let nmac = syn::Macro { tokens: nts, ..mac };

                    let nexp = syn::ExprMacro { mac: nmac, ..exp };
                    // nexp
                    syn::Expr::Macro(nexp)
                }
                t => t,
            };

            // let ret = syn::Stmt::Semi(nexp, semi);
            syn::Stmt::Semi(nexp, semi)
        }
        tt => tt,
    }
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
