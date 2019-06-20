#![recursion_limit="128"]

extern crate proc_macro;
use crate::proc_macro::TokenStream;
use quote::*;
use syn;

#[proc_macro_derive(SpecConstantStorage)]
pub fn spec_constant_storage_impl(tok: TokenStream) -> TokenStream {
    let input: syn::DeriveInput = syn::parse(tok).expect("Parsing");

    let name = &input.ident;
    let fields = if let syn::Data::Struct(syn::DataStruct { fields, .. }) = &input.data { fields } else {
        panic!("SpecConstantStorage can only be applied to struct");
    };
    let map_entry = match fields {
        syn::Fields::Named(syn::FieldsNamed { named, .. }) => {
            let (idents, tys): (Vec<_>, Vec<_>) = named.iter()
                .map(|f| (f.ident.as_ref().expect("unnamed?"), &f.ty)).unzip();
            let ids = idents.iter().enumerate().map(|x| x.0).collect::<Vec<_>>();
            
            quote! {
                vec![#(br::vk::VkSpecializationMapEntry {
                    constantID: #ids as _,
                    offset: unsafe {
                        std::mem::transmute::<_, usize>(&std::mem::transmute::<_, &Self>(0usize).#idents) as _
                    },
                    size: std::mem::size_of::<#tys>() as _
                }),*]
            }
        },
        syn::Fields::Unnamed(syn::FieldsUnnamed { unnamed, .. }) => {
            let (tys, ids): (Vec<_>, Vec<_>) = unnamed.iter().enumerate().map(|(n, f)| (&f.ty, n)).unzip();
            let ids2 = ids.iter().enumerate().map(|x| x.0).collect::<Vec<_>>();
            
            quote! {
                vec![#(br::vk::VkSpecializationMapEntry {
                    constantID: #ids2 as _,
                    offset: unsafe {
                        std::mem::transmute::<_, usize>(&std::mem::transmute::<_, &Self>(0usize).#ids) as _
                    },
                    size: std::mem::size_of::<#tys>() as _
                }),*]
            }
        },
        _ => quote! { vec![] }
    };

    TokenStream::from(quote! {
        impl SpecConstantStorage for #name {
            fn as_pair(&self) -> (Vec<br::vk::VkSpecializationMapEntry>, br::DynamicDataCell) {
                (#map_entry, br::DynamicDataCell::from(self))
            }
        }
    })
}
