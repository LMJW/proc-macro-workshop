extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

fn field_is_option(t: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { ref segments, .. },
        ..
    }) = t
    {
        if segments.len() == 1 && segments[0].ident == "Option" {
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
                    if let syn::GenericArgument::Type(ty)= args.last().unwrap(){
                        return Some(ty);
                    }
                }
            }
        };
    };
    None
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
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

    // eprintln!("{:#?}", extend);
    TokenStream::from(extend)
}
