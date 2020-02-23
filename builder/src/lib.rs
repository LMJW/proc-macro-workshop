extern crate proc_macro;
extern crate proc_macro2;

use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

fn field_is_option(t: &syn::Type) -> Option<&syn::Type> {
    get_inner_ty("Option".to_string(), t)
}

fn get_inner_ty(wrap_ty: String, t: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { ref segments, .. },
        ..
    }) = t
    {
        if segments.len() == 1 && segments[0].ident == wrap_ty.as_str(){
            if let syn::PathSegment {
                arguments:
                    syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                        ref args,
                        ..
                    }),
                ..
            } = segments[0]
            {
                if args.len() == 1 {
                    if let syn::GenericArgument::Type(ty) = args.last().unwrap() {
                        return Some(ty);
                    }
                }
            }
        }else{
            if segments.len() != 1{
                panic!("wrong numbers of segments length: {}", segments.len());
            }
            
        }
    };
    None
}



fn field_is_vector(f: &syn::Field) -> Option<(syn::Ident, syn::Type)> {
    // we will need to know two things for this. For example of "args" and
    // "arg", we need to know "arg" because the "arg" is method operate on
    // builder struct. We will also need to know the inner field of the vector,
    // which is the type of argument of the "arg" method

    // let's just handle the single case where there is only one macro
    // #[builder(each = "arg")] on the struct fields args, so we will only
    // consider the case where attrs vector equal to 1.

    if f.attrs.len() == 0 {
        return None;
    }

    if f.attrs.len() != 1 {
        unimplemented!();
    }
    
    let attr = f.attrs[0].clone();
    let ty = f.ty.clone();
    if let syn::Attribute { ref tokens, .. } = attr {
        let tts: Vec<TokenTree> = tokens.clone().into_iter().collect();

        if tts.len() == 1 {
            if let TokenTree::Group(grp) = &tts[0] {
                let stream = grp.stream();
                for s in stream.into_iter() {
                    match s {
                        TokenTree::Ident(ident) => {
                            // only defined for the 'each = "arg"' case
                            assert_eq!(ident.to_string(), "each");
                        }
                        TokenTree::Literal(lit) => {
                            // return a syn::Lit enum
                            let name = match syn::Lit::new(lit.clone()){
                                syn::Lit::Str(l)=> l,
                                _ =>{panic!("expecting string literal")},
                            };
                            let span = lit.span();
                            let ident = syn::Ident::new(name.value().as_str(), span);
                            // this creates the ident(eg: arg / env)
                            
                            // now need to get the inner type of Vec<String>,
                            // which is `String`
                            let inner_ty = get_inner_ty("Vec".to_string(), &ty).cloned().unwrap();
                            
                            return Some((ident, inner_ty));
                        }
                        TokenTree::Punct(p) => {
                            assert_eq!(p.as_char(), '=');
                        }
                        _ => { /* ignore other case */ }
                    }
                }
            }
        }
        unimplemented!();
    };
    None
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident.clone();
    let bname = format!("{}Builder", name);
    let bident = syn::Ident::new(bname.as_str(), name.span());

    // this is similar to the destructor in javascript
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = input.data
    {
        named
    } else {
        unimplemented!()
    };

    let builder_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        // for the builder field, we want each field is wrapped by the Option
        // type, so if the original field is option type, we will keep the
        // original type, otherwise, we will wrap the orginal type with Option
        // type
        if field_is_option(ty).is_some() {
            quote! {
                // this is how each field is mapped to the new option type field
                // eg: x: String => x:Option<String>
                #name : #ty
            }
        } else {
            quote! {
                // this is how each field is mapped to the new option type field
                // eg: x: String => x:Option<String>
                #name : std::option::Option<#ty>
            }
        }
    });

    let builder_default = fields.iter().map(|f| {
        let name = &f.ident;
        // Because all the fields are the Optional type, we do not need to check
        // the types, we will just pass in the None type
        quote! {
            // this is how each field is mapped to the new option type field
            // eg: x: String => x:Option<String>
            #name : std::option::Option::None
        }
    });

    let builder_impl = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if let Some(inner_ty) = field_is_option(ty) {
            // what to return?
            // need to unwrap #ty to inner type if ty is an option
            quote! {
                pub fn #name(&mut self, #name:#inner_ty) -> &mut Self{
                    self.#name = Some(#name);
                    self
                }
            }
        } else if let Some(tup) = field_is_vector(&f) {
            let (ident, ty) = tup;
            // eprintln!("{:?}----{:?}",identx,identy);
            quote! {
                pub fn #ident(&mut self, #ident:#ty)->&mut Self{
                    
                    if let Some(v) = &mut self.#name{
                        v.push(#ident); 
                    }else{
                        self.#name = Some(Vec::from(vec![#ident]));
                    }
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name:#ty) -> &mut Self{
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });

    let field_iter = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if field_is_option(ty).is_some() {
            quote! {
                #name:self.#name.clone()
            }
        } else if  field_is_vector(&f).is_some(){
            quote!{
                #name:self.#name.clone().unwrap_or(vec![])
            }
        }else {
            quote! {
                #name:self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let extend = quote! {
        pub struct #bident{
            // expand the optimizer iterator to here
            // #(...)*
            #(#builder_fields,)*
        }

        impl #name{
            fn builder() -> #bident{
                #bident{
                    // the default builder fields will be None
                    #(#builder_default,)*
                }
            }
        }

        impl #bident{
            #(#builder_impl)*

            pub fn build(&mut self)->Result<#name, Box<dyn std::error::Error>>{
                Ok(#name{
                    #(#field_iter,)*
                })
            }
        }
    };

    // eprintln!("{:#?}", input);
    TokenStream::from(extend)
}
