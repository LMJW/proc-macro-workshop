extern crate proc_macro;
extern crate proc_macro2;

use proc_macro::TokenStream;
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
        if segments.len() == 1 && segments[0].ident == wrap_ty.as_str() {
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
        } else {
            if segments.len() != 1 {
                panic!("wrong numbers of segments length: {}", segments.len());
            }
        }
    };
    None
}

fn mk_err<T: quote::ToTokens>(t: T) -> proc_macro2::TokenStream {
    syn::Error::new_spanned(t, "expected `builder(each = \"...\")`").to_compile_error()
}

#[allow(irrefutable_let_patterns)]
fn field_is_vector(f: &syn::Field) -> (syn::Ident, Option<proc_macro2::TokenStream>) {
    // the current api for this function is annoying, where the output will be either an
    // Option in success case, and a tokenstream when error happens. However,
    // for the success case, we will have Option None or Option Some. The Option
    // Some is what we want, but for Option None, it is still the okay case. So
    // the current function name is not really the meaning of it.

    // for now, we only implemented the attrs length to be 1, not other cases
    let attrs = &f.attrs;
    
    assert_eq!(attrs.len(), 1);
    let mut err_ret = None;

    let attr = attrs[0].clone();
    let meta = attr.parse_meta();
    let m = match meta {
        Ok(syn::Meta::List(mut metalist)) => {
            if metalist.nested.len() != 1 {
                err_ret = Some(mk_err(metalist.clone()));
            }
            match metalist.nested.pop().unwrap().into_value() {
                syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) => {
                    let ident = nv.path.get_ident().unwrap();
                    if ident.to_string() != "each".to_string() {
                        err_ret = Some(mk_err(metalist));
                    }
                    nv
                }
                _ => {
                    // nvs.nested[0] was not k = v
                    // err_ret = Some(mk_err(meta));
                    panic!()
                }
            }
        }
        Ok(_) => {
            // err_ret = Some(mk_err(meta));
            panic!()
        }
        Err(_) => {
            // err_ret = Some(e.to_compile_error());
            panic!()
        }
    };
    // eprintln!("{:#?}", m);
    match m.lit {
        syn::Lit::Str(s) => {
            let arg = syn::Ident::new(&s.value(), s.span());
            // let inner_ty = get_inner_ty("Vec".to_string(), &f.ty).unwrap();
            // let method = quote! {
            //     pub fn #arg(&mut self, #arg: #inner_ty) -> &mut Self {
            //         self.#name.push(#arg);
            //         self
            //     }
            // };
            (arg, err_ret)
        }
        lit => panic!("expected string, found {:?}", lit),
    }
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
        } else if *(&f.attrs.len()) == 1usize {
            let (ident, errs) = field_is_vector(&f);
            let inner_ty = get_inner_ty("Vec".to_string(), ty);

            match errs {
                Some(e) => {
                    quote! {
                        pub fn #ident(&mut self, #ident:#inner_ty)->&mut Self{

                            if let Some(v) = &mut self.#name{
                                v.push(#ident);
                            }else{
                                self.#name = Some(Vec::from(vec![#ident]));
                            }
                            self
                        }
                        #e
                    }
                }
                None => {
                    quote! {
                        pub fn #ident(&mut self, #ident:#inner_ty)->&mut Self{

                            if let Some(v) = &mut self.#name{
                                v.push(#ident);
                            }else{
                                self.#name = Some(Vec::from(vec![#ident]));
                            }
                            self
                        }
                    }
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
        } else if &f.attrs.len() == &1usize {
            quote! {
                #name:self.#name.clone().unwrap_or(vec![])
            }
        } else {
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


        impl #bident{
            #(#builder_impl)*

            pub fn build(&mut self)->std::result::Result<#name, std::boxed::Box<dyn std::error::Error>>{
                std::result::Result::Ok(#name{
                    #(#field_iter,)*
                })
            }
        }

        impl #name{
            pub fn builder() -> #bident{
                #bident{
                    // the default builder fields will be None
                    #(#builder_default,)*
                }
            }
        }

    };

    // eprintln!("{:#?}", extend);
    TokenStream::from(extend)
}
