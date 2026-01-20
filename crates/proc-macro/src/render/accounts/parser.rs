use base64::{engine::general_purpose::STANDARD, Engine};
use codama_nodes::{
    CamelCaseString, DiscriminatorNode, NestedTypeNode, Number, TypeNode, ValueNode,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/**
 * Build the *account parser* for a program.
 */
pub fn parser(
    program_name_camel: &CamelCaseString,
    accounts: &[codama_nodes::AccountNode],
) -> TokenStream {
    let program_name = crate::utils::to_pascal_case(program_name_camel);

    let account_enum_ident = format_ident!("{}Account", program_name);

    let parser_id = format!("{}::AccountParser", program_name);

    let parser_error_msg = format!("Unknown account for program {}", program_name);

    // List all the accounts as enum variants
    let account_enum_fields = accounts.iter().map(|account| {
        let account_ident = format_ident!("{}", crate::utils::to_pascal_case(&account.name));

        quote! { #account_ident(::prost::alloc::vec::Vec<u8>) }
    });

    let account_matches = accounts.iter().filter_map(|account| {
        let discriminator = account.discriminators.first()?;
        let account_ident = format_ident!("{}", crate::utils::to_pascal_case(&account.name));

        Some(match discriminator {
            // Handle 1 byte discriminators (simple programs like SPL Token)
            DiscriminatorNode::Constant(node) => {
                let offset = node.offset;

                // Skip if not a number
                let ValueNode::Number(nn) = node.constant.value.as_ref() else {
                    return None;
                };

                // Skip if not an unsigned integer
                let Number::UnsignedInteger(value) = nn.number else {
                    return None;
                };

                quote! {
                    if let Some(discriminator) = data.get(#offset) {
                        if discriminator == #value {
                            return Ok(#account_enum_ident::#account_ident(data.to_vec()));
                        }
                    }
                }
            },

            // Handle multi-byte discriminators (like Anchor's 8 byte discriminators)
            DiscriminatorNode::Field(node) => {
                let offset = node.offset;

                // Skip if not a struct
                let NestedTypeNode::Value(struct_node) = &account.data else {
                    return None;
                };

                // Find the discriminator field by name
                let field = struct_node.fields.iter().find(|f| f.name == node.name)?;

                // Skip if discriminator field isn't fixed-size bytes
                let TypeNode::FixedSize(fixed_size_node) = &field.r#type else {
                    return None;
                };

                let size = fixed_size_node.size;

                // Skip if no default value
                let default_value = field.default_value.as_ref()?;

                // Skip if default value isn't bytes
                let ValueNode::Bytes(bytes) = default_value else {
                    return None;
                };

                // Decode expected discriminator bytes
                let discriminator: Vec<u8> = match bytes.encoding {
                    codama_nodes::BytesEncoding::Base16 => {
                        hex::decode(&bytes.data).expect("Failed to decode base16 (hex) bytes")
                    },
                    codama_nodes::BytesEncoding::Base58 => bs58::decode(&bytes.data)
                        .into_vec()
                        .expect("Failed to decode base58 bytes"),
                    codama_nodes::BytesEncoding::Base64 => STANDARD
                        .decode(&bytes.data)
                        .expect("Failed to decode base64 bytes"),
                    codama_nodes::BytesEncoding::Utf8 => bytes.data.as_bytes().to_vec(),
                };

                let end = offset + size;

                quote! {
                    if let Some(slice) = data.get(#offset..#end) {
                        if slice == &[#(#discriminator),*] {
                            return Ok(#account_enum_ident::#account_ident(data[#end..].to_vec()));
                        }
                    }
                }
            },

            // Handle accounts based on size only (e.g the account is 558 Bytes long)
            DiscriminatorNode::Size(node) => {
                let size = node.size;

                quote! {
                    if data.len() == #size {
                        return Ok(#account_enum_ident::#account_ident(data.to_vec()));
                    }
                }
            },
        })
    });

    quote! {
        #[derive(Debug)]
        pub enum #account_enum_ident {
            #(#account_enum_fields),*
        }

        impl #account_enum_ident {
            pub fn try_unpack(data: &[u8]) -> ParseResult<Self> {
                #(#account_matches)*
                Err(ParseError::from(#parser_error_msg.to_owned()))
            }
        }

        #[derive(Debug, Copy, Clone)]
        pub struct AccountParser;

        impl Parser for AccountParser {
            type Input = AccountUpdate;
            type Output = #account_enum_ident;

            fn id(&self) -> std::borrow::Cow<'static, str> {
                #parser_id.into()
            }

            fn prefilter(&self) -> Prefilter {
                Prefilter::builder()
                    .account_owners([ID_BYTES])
                    .build()
                    .unwrap()
            }

            async fn parse(&self, acct: &AccountUpdate) -> ParseResult<Self::Output> {
                let inner = acct
                    .account
                    .as_ref()
                    .ok_or_else(|| ParseError::from("Unable to unwrap account ref".to_owned()))?;

                #account_enum_ident::try_unpack(&inner.data)
            }
        }
    }
}
