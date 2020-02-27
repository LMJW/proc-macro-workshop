extern crate proc_macro;
extern crate proc_macro2;

extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use proc_macro2;

use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Result, Token};

use quote::quote;

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    // let _ = input;
    let _input = parse_macro_input!(input as SeqMacroInput);

    TokenStream::new()
}

struct SeqMacroInput {
    /* ... */
    ident: syn::Ident,
    intok: Token![in],
    starti: syn::LitInt,
    dottok: Token![..],
    endi: syn::LitInt,
    block: syn::Block,
}

impl Parse for SeqMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: syn::Ident = input.parse()?;
        let intok: Token![in] = input.parse()?;
        let starti: syn::LitInt = input.parse()?;
        let dottok: Token![..] = input.parse()?;
        let endi: syn::LitInt = input.parse()?;

        let block: syn::Block = input.parse()?;
        let content = block.stmts.clone();
        eprintln!("{:#?}", content);

        let a = starti.base10_parse::<u16>()?;
        let b = endi.base10_parse::<u16>()?;

        let out = proc_macro2::TokenStream::new();
        for i in a..b{
            let out : Vec<_>= content.into_iter().map(|stmt|{expand(i, stmt)}).collect();

        }

        Ok(SeqMacroInput {
            ident,
            intok,
            starti,
            dottok,
            endi,
            block,
        })
    }
}

fn expand(n:u16, expr:syn::Stmt)->proc_macro2::TokenStream{
    match expr {
        syn::Stmt::Semi(expr, semi) =>{
            let nexp = if let syn::Expr::Macro(exp) = expr{
                let attr = exp.attrs;
                let macro = exp.mac;


            }else{
                unimplemented!();
            }

            let ret = syn::Stmt::Semi(nexp, semi);
            quote!{
                #ret
            }
        }
        tt=>quote!{#tt}
    }
}

fn expand2(n:u16,tt:proc_macro2::TokenTree)->proc_macro2::TokenTree{
    match tt{
        proc_macro2::TokenTree::Group(g)=>{
            let ns :Vec<_> = g.stream().into_iter().map(|it|{
                match it{
                    proc_macro2::TokenTree::Group(gg)=>{
                        let tts: Vec<_> = gg.stream().into_iter().map(|item|{
                            match item{
                                proc_macro2::TokenTree::Ident(ident) =>{
                                    let ns = format!("{}", n);
                                    let ntk = proc_macro2::Ident::new(ns.as_str(), ident.span());
                                    ntk
                                }
                                _ =>_
                            }
                        }).collect();
                        proc_macro2::Group::new(gg.delimiter(),)
                    }
                    xx=>xx
                }
            }).collect();
            let mut ng = proc_macro2::TokenTree::new(g.delimiter(), ns);
            proc_macro2::TokenTree::Group()
        }
        t=>t
    }
}