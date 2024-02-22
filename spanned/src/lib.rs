use quote::quote;

#[proc_macro_derive(Spannable)]
pub fn spannable_macro_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_spannable_macro(&ast)
}

fn impl_spannable_macro(ast: &syn::DeriveInput) -> proc_macro::TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl Spannable for #name {
            fn span(&self) -> &Span {
                &self.span
            }
        }
    };
    gen.into()
}
