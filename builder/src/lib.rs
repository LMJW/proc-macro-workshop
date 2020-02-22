extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

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
        quote! {
            // this is how each field is mapped to the new option type field
            // eg: x: String => x:Option<String>
            #name : std::option::Option<#ty>
        }
    });

    let builder_default = fields.iter().map(|f| {
        let name = &f.ident;
        quote! {
            // this is how each field is mapped to the new option type field
            // eg: x: String => x:Option<String>
            #name : std::option::Option::None
        }
    });

    let builder_impl = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote! {
            pub fn #name(&mut self, #name:#ty) -> &mut Self{
                self.#name = Some(#name);
                self
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
            // pub fn executable(&mut self, executable: String) -> &mut Self{
            //     self.executable = Some(executable);
            //     self
            // }

            // pub fn args(&mut self, args: Vec<String>) -> &mut Self{
            //     self.args = Some(args);
            //     self
            // }

            // pub fn env(&mut self, env: Vec<String>) -> &mut Self{
            //     self.env = Some(env);
            //     self
            // }

            // pub fn current_dir(&mut self, current_dir: String) -> &mut Self{
            //     self.current_dir = Some(current_dir);
            //     self
            // }

            pub fn build(&mut self)->Result<#name, Box<dyn std::error::Error>>{
                Ok(#name{
                    executable:self.executable.clone().ok_or("missing executable")?,
                    args:self.args.clone().ok_or("missing args")?,
                    env:self.env.clone().ok_or("missing env")?,
                    current_dir:self.current_dir.clone().ok_or("missing current_dir")?,
                })
            }
        }
    };

    // eprintln!("{:#?}", extend);
    TokenStream::from(extend)
}
