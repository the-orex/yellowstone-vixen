use codama_nodes::RootNode;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub fn vixen_parser(idl: &RootNode) -> TokenStream {
    let program_mod_ident = format_ident!("{}", crate::utils::to_snake_case(&idl.program.name));

    let program_pubkey = crate::render::program_pubkey(&idl.program.public_key);

    let defined_types = crate::render::defined_types(&idl.program.defined_types);

    let account_types = crate::render::accounts::types(&idl.program.accounts);
    let instruction_types = crate::render::instructions::types(&idl.program.instructions);

    let account_parser = crate::render::accounts::parser(&idl.program.name, &idl.program.accounts);
    let instruction_parser =
        crate::render::instructions::parser(&idl.program.name, &idl.program.instructions);

    quote! {
        mod #program_mod_ident {
            use yellowstone_vixen_parser::prelude::*;

            /// 32 bytes by convention.
            pub type PubkeyBytes = ::prost::alloc::vec::Vec<u8>;

            /// Program id bytes as a const (no allocations).
            pub const ID_BYTES: [u8; 32] = #program_pubkey;

            #defined_types
            #account_types
            #instruction_types
            #account_parser
            #instruction_parser
        }
    }
}
