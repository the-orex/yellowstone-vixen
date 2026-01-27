use codama_nodes::NestedTypeNode;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/**
 * Generate protobuf-compatible message types for program accounts.
 *
 * Example (roughly what gets generated):
 *
 *   #[derive(Clone, PartialEq, ::prost::Message)]
 *   pub struct GlobalLimitsTuple {
 *     #[prost(uint64, tag = 1)]
 *     pub item_0: u64,
 *     #[prost(string, tag = 2)]
 *     pub item_1: ::prost::alloc::string::String,
 *   }
 *
 *   #[derive(Clone, PartialEq, ::prost::Message)]
 *   pub struct Global {
 *     #[prost(bytes = "vec", tag = 1)]
 *     pub authority: ::prost::alloc::vec::Vec<u8>, // PubkeyBytes alias upstream
 *     #[prost(uint32, tag = 2)]
 *     pub fee_bps: u32,
 *     #[prost(message, optional, tag = 3)]
 *     pub limits: ::core::option::Option<GlobalLimitsTuple>,
 *   }
 */
pub fn types(accounts: &[codama_nodes::AccountNode]) -> TokenStream {
    let sections = accounts.iter().map(|account| {
        let struct_ident = format_ident!("{}", crate::utils::to_pascal_case(&account.name));

        let (extra_defs, fields) = match &account.data {
            NestedTypeNode::Value(struct_type) => {
                crate::render::defined_types::render_struct_fields(
                    &struct_ident,
                    &struct_type.fields,
                )
            },
            _ => (TokenStream::new(), Vec::new()),
        };

        let len_const = account
            .size
            .map(|size| quote! { pub const LEN: usize = #size; });

        quote! {
            #extra_defs

            #[derive(Clone, PartialEq, ::prost::Message)]
            pub struct #struct_ident {
                #(#fields),*
            }

            impl #struct_ident {
                #len_const
            }
        }
    });

    quote! { #(#sections)* }
}
