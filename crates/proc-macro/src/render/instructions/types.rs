use codama_nodes::{DefaultValueStrategy, InstructionNode};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/**
 * Generate a proto compatible type wrapping all instructions definitions in an struct
 *
 * Minimal generated example (for instructions: Create, Sell):
 *
 *   #[derive(Clone, PartialEq, ::prost::Message)]
 *   pub struct CreateIx {
 *       #[prost(message, optional, tag = "1")]
 *       pub accounts: Option<CreateAccounts>,
 *       #[prost(message, optional, tag = "2")]
 *       pub args: Option<CreateArgs>,
 *   }
 *
 *   #[derive(Clone, PartialEq, ::prost::Message)]
 *   pub struct SellIx {
 *       #[prost(message, optional, tag = "1")]
 *       pub accounts: Option<SellAccounts>,
 *       #[prost(message, optional, tag = "2")]
 *       pub args: Option<SellArgs>,
 *   }
 *
 *   #[derive(Clone, PartialEq, ::prost::Message)]
 *   pub struct ProgramInstruction {
 *       #[prost(oneof = "program_instruction_oneof::Ix", tags = "1, 2")]
 *       pub ix: Option<program_instruction_oneof::Ix>,
 *   }
 *
 *   pub mod program_instruction_oneof {
 *       #[derive(Clone, PartialEq, ::prost::Oneof)]
 *       pub enum Ix {
 *           #[prost(message, tag = "1")]
 *           CreateIx(super::CreateIx),
 *           #[prost(message, tag = "2")]
 *           SellIx(super::SellIx),
 *       }
 *   }
 */
pub fn instruction_envelope_type(instructions: &[InstructionNode]) -> TokenStream {
    let program_instruction_ident = format_ident!("ProgramInstruction");
    let oneof_mod_ident = format_ident!("program_instruction_oneof");
    let oneof_ident = format_ident!("Ix");

    // Generate tags list: "1, 2, 3, ..."
    let tags_list = {
        let tags_list = (1..=instructions.len())
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        syn::LitStr::new(&tags_list, proc_macro2::Span::call_site())
    };

    let oneof_path = {
        let oneof_path = format!("{}::{}", oneof_mod_ident, oneof_ident);
        syn::LitStr::new(&oneof_path, proc_macro2::Span::call_site())
    };

    let instruction_payload_msgs = instructions.iter().map(|ix| {
        let ix_name = crate::utils::to_pascal_case(&ix.name);

        let payload_ident = format_ident!("{}Ix", ix_name);
        let accounts_ident = format_ident!("{}Accounts", ix_name);
        let args_ident = format_ident!("{}Args", ix_name);

        quote! {
            #[derive(Clone, PartialEq, ::prost::Message)]
            pub struct #payload_ident {
                #[prost(message, optional, tag="1")]
                pub accounts: ::core::option::Option<#accounts_ident>,

                #[prost(message, optional, tag="2")]
                pub args: ::core::option::Option<#args_ident>,
            }
        }
    });

    let oneof_variants = instructions.iter().enumerate().map(|(i, ix)| {
        let tag = (i + 1) as u32;
        let ix_name = crate::utils::to_pascal_case(&ix.name);
        let payload_ident = format_ident!("{}Ix", ix_name);

        quote! { #[prost(message, tag = #tag)] #payload_ident(super::#payload_ident) }
    });

    quote! {
        #(#instruction_payload_msgs)*

        #[derive(Clone, PartialEq, ::prost::Message)]
        pub struct #program_instruction_ident {
            #[prost(oneof = #oneof_path, tags = #tags_list)]
            pub ix: ::core::option::Option<#oneof_mod_ident::#oneof_ident>,
        }

        pub mod #oneof_mod_ident {
            #[derive(Clone, PartialEq, ::prost::Oneof)]
            pub enum #oneof_ident {
                #(#oneof_variants),*
            }
        }
    }
}

/**
 * Generate protobuf compatible type for one instruction
 *
 * Minimal generated example (instruction: `Create`):
 *
 *   #[derive(Clone, PartialEq, ::prost::Message)]
 *   pub struct CreateAccounts {
 *       #[prost(bytes = "vec", tag = "1")]
 *       pub payer: PubkeyBytes,
 *       #[prost(bytes = "vec", tag = "2")]
 *       pub mint: PubkeyBytes,
 *   }
 *
 *   #[derive(Clone, PartialEq, ::prost::Message, ::borsh::BorshDeserialize, ::borsh::BorshSerialize)]
 *   pub struct CreateArgs {
 *       #[prost(uint64, tag = "1")]
 *       pub supply: u64,
 *       #[prost(string, tag = "2")]
 *       pub name: ::prost::alloc::string::String,
 *   }
 */
fn single_instruction_type(instruction: &InstructionNode) -> TokenStream {
    let instr_pascal = crate::utils::to_pascal_case(&instruction.name);

    let accounts_ident = format_ident!("{}Accounts", instr_pascal);
    let args_ident = format_ident!("{}Args", instr_pascal);

    /* Accounts: always pubkeys (bytes) */
    let accounts_fields = instruction.accounts.iter().enumerate().map(|(i, account)| {
        let tag = (i + 1) as u32;
        let field_ident = format_ident!("{}", crate::utils::to_snake_case(&account.name));

        quote! {
            #[prost(bytes = "vec", tag = #tag)]
            pub #field_ident: PubkeyBytes
        }
    });

    let mut args_extra_defs = TokenStream::new();
    let mut args_fields = Vec::new();

    for (i, arg) in instruction.arguments.iter().enumerate() {
        if arg.default_value_strategy == Some(DefaultValueStrategy::Omitted) {
            continue;
        }

        let tag = (i + 1) as u32;
        let field_ident = format_ident!("{}", crate::utils::to_snake_case(&arg.name));

        let rendered = crate::render::defined_types::render_field_with_helpers(
            &args_ident,
            &field_ident,
            tag,
            &arg.r#type,
        );

        args_extra_defs.extend(rendered.extra_defs);
        args_fields.push(rendered.field);
    }

    quote! {
        #[derive(Clone, PartialEq, ::prost::Message)]
        pub struct #accounts_ident {
            #(#accounts_fields),*
        }

        #args_extra_defs

        #[derive(Clone, PartialEq, ::prost::Message, ::borsh::BorshDeserialize, ::borsh::BorshSerialize)]
        pub struct #args_ident {
            #(#args_fields),*
        }
    }
}

/**
 * Generate protobuf compatible types for instructions
 */
pub fn types(instructions: &[InstructionNode]) -> TokenStream {
    let sections = instructions.iter().map(single_instruction_type);
    let instruction_envelope_type = instruction_envelope_type(instructions);

    quote! {
        #(#sections)*

        #instruction_envelope_type
    }
}
