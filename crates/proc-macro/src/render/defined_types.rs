use codama_nodes::{DefaultValueStrategy, EnumVariantTypeNode, TypeNode};
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::LitStr;

pub fn defined_types(defined_types: &[codama_nodes::DefinedTypeNode]) -> TokenStream {
    let sections = defined_types.iter().map(|defined_type| {
        let ident = format_ident!("{}", crate::utils::to_pascal_case(&defined_type.name));

        match &defined_type.r#type {
            TypeNode::Struct(struct_type) => render_defined_struct(&ident, struct_type),
            TypeNode::Enum(enum_type) => render_defined_enum(&ident, enum_type),
            TypeNode::Tuple(tuple) => render_defined_tuple(&ident, tuple),
            other => render_other_defined_type(&ident, other),
        }
    });

    quote! { #(#sections)* }
}

/**
 * Generates code looking like:
 *
 * #[derive(Clone, PartialEq, ::prost::Message, ::borsh::BorshDeserialize, ::borsh::BorshSerialize)]
 * pub struct TypeName {
 *     #[prost(..., tag = N)]
 *     pub field_name: field_type,
 *     ...
 * }
 *
 * Nested tuple fields may generate additional helper message definitions
 * that are emitted before the struct itself.
 */
fn render_defined_struct(
    ident: &syn::Ident,
    struct_type: &codama_nodes::StructTypeNode,
) -> TokenStream {
    let (extra_defs, fields) = render_struct_fields(ident, &struct_type.fields);

    quote! {
        #extra_defs
        #[derive(Clone, PartialEq, ::prost::Message, ::borsh::BorshDeserialize, ::borsh::BorshSerialize)]
        pub struct #ident {
            #(#fields),*
        }
    }
}

/**
 * Generates code looking like:
 *
 * #[derive(Clone, PartialEq, ::prost::Message, ::borsh::BorshDeserialize, ::borsh::BorshSerialize)]
 * pub struct EnumName {
 *     #[prost(oneof = "enum_name::Kind", tags = "1, 2, 3")]
 *     pub kind: ::core::option::Option<enum_name::Kind>,
 * }
 *
 * pub mod enum_name {
 *     #[derive(Clone, PartialEq, ::prost::Oneof)]
 *     pub enum Kind {
 *         #[prost(message, tag = 1)]
 *         VariantA(super::VariantA),
 *
 *         #[prost(message, tag = 2)]
 *         VariantB(super::VariantB),
 *
 *         #[prost(message, tag = 3)]
 *         VariantC(super::VariantC),
 *     }
 * }
 *
 * #[derive(Clone, PartialEq, ::prost::Message, ::borsh::BorshDeserialize, ::borsh::BorshSerialize)]
 * pub struct VariantA {}
 *
 * #[derive(Clone, PartialEq, ::prost::Message, ::borsh::BorshDeserialize, ::borsh::BorshSerialize)]
 * pub struct VariantB {
 *     #[prost(uint64, tag = 1)]
 *     pub amount: u64,
 * }
 *
 * #[derive(Clone, PartialEq, ::prost::Message, ::borsh::BorshDeserialize, ::borsh::BorshSerialize)]
 * pub struct VariantC {
 *     #[prost(string, tag = 1)]
 *     pub name: ::prost::alloc::string::String,
 *     #[prost(uint32, tag = 2)]
 *     pub count: u32,
 * }
 */
fn render_defined_enum(
    enum_ident: &proc_macro2::Ident,
    enum_type: &codama_nodes::EnumTypeNode,
) -> TokenStream {
    let mod_ident = format_ident!(
        "{}",
        crate::utils::to_snake_case_str(enum_ident.to_string().as_str())
    );
    let oneof_ident = format_ident!("Kind");

    // prost wants a string literal for tags = "1, 2, 3"
    let tags_list = (1..=enum_type.variants.len())
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    let tags_lit = LitStr::new(&tags_list, Span::call_site());

    let mut payload_defs = TokenStream::new();
    let mut empty_variant_msgs = TokenStream::new();

    for variant in &enum_type.variants {
        match variant {
            EnumVariantTypeNode::Empty(v) => {
                let v_ident = format_ident!("{}", crate::utils::to_pascal_case(&v.name));
                empty_variant_msgs.extend(quote! {
                    #[derive(Clone, PartialEq, ::prost::Message, ::borsh::BorshDeserialize, ::borsh::BorshSerialize)]
                    pub struct #v_ident {}
                });
            },

            EnumVariantTypeNode::Tuple(v) => {
                let v_ident = format_ident!("{}", crate::utils::to_pascal_case(&v.name));

                let tuple = match &v.tuple {
                    codama_nodes::NestedTypeNode::Value(t) => t,
                    _ => panic!("Expected TupleTypeNode::Value in EnumTupleVariantTypeNode.tuple"),
                };

                let mut extra_defs = TokenStream::new();
                let mut fields = Vec::new();

                for (i, item) in tuple.items.iter().enumerate() {
                    let tag = (i + 1) as u32;
                    let name = format_ident!("item_{}", i);

                    let rendered = render_field_with_helpers(&v_ident, &name, tag, item);
                    extra_defs.extend(rendered.extra_defs);
                    fields.push(rendered.field);
                }

                payload_defs.extend(quote! {
                    #extra_defs
                    #[derive(Clone, PartialEq, ::prost::Message, ::borsh::BorshDeserialize, ::borsh::BorshSerialize)]
                    pub struct #v_ident {
                        #(#fields),*
                    }
                });
            },

            EnumVariantTypeNode::Struct(v) => {
                let v_ident = format_ident!("{}", crate::utils::to_pascal_case(&v.name));

                let st = match &v.r#struct {
                    codama_nodes::NestedTypeNode::Value(st) => st,
                    _ => panic!(
                        "Expected StructTypeNode::Value in EnumStructVariantTypeNode.r#struct"
                    ),
                };

                let (extra_defs, fields) = render_struct_fields(&v_ident, &st.fields);

                payload_defs.extend(quote! {
                    #extra_defs
                    #[derive(Clone, PartialEq, ::prost::Message, ::borsh::BorshDeserialize, ::borsh::BorshSerialize)]
                    pub struct #v_ident {
                        #(#fields),*
                    }
                });
            },
        }
    }

    let oneof_variants = enum_type.variants.iter().enumerate().map(|(i, variant)| {
        let tag = (i + 1) as u32;

        match variant {
            EnumVariantTypeNode::Empty(v) => {
                let v_ident = format_ident!("{}", crate::utils::to_pascal_case(&v.name));
                quote! { #[prost(message, tag = #tag)] #v_ident(super::#v_ident) }
            },
            EnumVariantTypeNode::Tuple(v) => {
                let v_ident = format_ident!("{}", crate::utils::to_pascal_case(&v.name));
                quote! { #[prost(message, tag = #tag)] #v_ident(super::#v_ident) }
            },
            EnumVariantTypeNode::Struct(v) => {
                let v_ident = format_ident!("{}", crate::utils::to_pascal_case(&v.name));
                quote! { #[prost(message, tag = #tag)] #v_ident(super::#v_ident) }
            },
        }
    });

    quote! {
        #empty_variant_msgs
        #payload_defs

        #[derive(Clone, PartialEq, ::prost::Message)]
        pub struct #enum_ident {
            #[prost(oneof = #mod_ident::#oneof_ident, tags = #tags_lit)]
            pub kind: ::core::option::Option<#mod_ident::#oneof_ident>,
        }

        pub mod #mod_ident {
            #[derive(Clone, PartialEq, ::prost::Oneof)]
            pub enum #oneof_ident {
                #(#oneof_variants),*
            }
        }
    }
}

/**
 * Tuples are not directly supported by protobuf, so they are lowered
 * into a synthetic `Message` with fields named `item_0`, `item_1`, ...
 *
 * Example:
 *
 * pub struct TupleName {
 *     #[prost(..., tag = 1)]
 *     pub item_0: T0,
 *     #[prost(..., tag = 2)]
 *     pub item_1: T1,
 * }
 *
 * Nested tuple elements may generate additional helper message definitions.
 */
fn render_defined_tuple(ident: &syn::Ident, tuple: &codama_nodes::TupleTypeNode) -> TokenStream {
    // Top-level tuple defined-type => represent as a Message with item_0..item_n.
    let mut extra_defs = TokenStream::new();
    let mut tuple_fields = Vec::new();

    for (i, item) in tuple.items.iter().enumerate() {
        let tag = (i + 1) as u32;
        let name = format_ident!("item_{}", i);

        let rendered = render_field_with_helpers(ident, &name, tag, item);
        extra_defs.extend(rendered.extra_defs);
        tuple_fields.push(rendered.field);
    }

    quote! {
        #extra_defs
        #[derive(Clone, PartialEq, ::prost::Message, ::borsh::BorshDeserialize, ::borsh::BorshSerialize)]
        pub struct #ident {
            #(#tuple_fields),*
        }
    }
}

/**
 * Generate a type alias for non-struct, non-enum, non-tuple defined-types.
 *
 * This is used for scalar or transparent types such as:
 * - string
 * - bytes
 * - numbers
 * - booleans
 * - fixed-size byte arrays
 *
 * Example output:
 *
 * pub type MyAlias = u32;
 */
fn render_other_defined_type(
    ident: &syn::Ident,
    type_node: &codama_nodes::TypeNode,
) -> TokenStream {
    let (_attr, ty) = map_codama_type_to_prost(type_node);

    quote! {
        pub type #ident = #ty;
    }
}

/**
 * Result of rendering a single struct field.
 *
 * Some IDL fields expand into more than one thing.
 *
 * Example:
 *   bar: (u64, string)
 *
 * Generates:
 *   struct FooBarTuple { ... }
 *   pub bar: Option<FooBarTuple>
 *
 * So we return:
 * - extra_defs: helper Message definitions
 * - field: the actual struct field
 */
pub struct ProstFieldTokens {
    pub extra_defs: TokenStream,
    pub field: TokenStream,
}

/**
 * Render all fields of a struct.
 *
 * Returns:
 * - extra_defs: helper Messages (tuple wrappers, etc.)
 * - fields: `#[prost(...)] pub field: Type` tokens
 *
 * `parent_ident` is used to name nested tuple messages
 * (e.g. Foo + bar => FooBarTuple).
 */
pub fn render_struct_fields(
    parent_ident: &proc_macro2::Ident,
    fields: &[codama_nodes::StructFieldTypeNode],
) -> (TokenStream, Vec<TokenStream>) {
    let mut extra_defs = TokenStream::new();
    let mut out_fields = Vec::new();

    for (i, f) in fields.iter().enumerate() {
        if f.default_value_strategy == Some(DefaultValueStrategy::Omitted) {
            continue;
        }

        let field_ident = format_ident!("{}", crate::utils::to_snake_case(&f.name));

        let tag = (i + 1) as u32;

        let rendered = render_field_with_helpers(parent_ident, &field_ident, tag, &f.r#type);

        extra_defs.extend(rendered.extra_defs);
        out_fields.push(rendered.field);
    }

    (extra_defs, out_fields)
}

/**
 * Render a single field into prost-compatible tokens. Handle recursive cases.
 *
 * Input is a Codama `TypeNode` attached to some parent message (struct / enum variant / tuple wrapper).
 *
 * Handles:
 * - Tuple field => generate a helper Message (ParentFieldTuple) and store as Option<...>
 * - Link field  => prost message fields must be Option<T>
 * - Scalar/bytes/repeated/option => emitted directly
 *
 * Returns:
 * - extra_defs: helper Message definitions (tuple wrappers, nested helpers)
 * - field:      the `#[prost(...)] pub name: Ty` token
 */
pub fn render_field_with_helpers(
    parent_ident: &proc_macro2::Ident,
    field_ident: &proc_macro2::Ident,
    tag: u32,
    type_node: &codama_nodes::TypeNode,
) -> ProstFieldTokens {
    use codama_nodes::TypeNode as T;

    match type_node {
        // Inline tuple field => generate nested message + Option<...> message field.
        T::Tuple(tuple) => {
            let tuple_ident = format_ident!(
                "{}{}Tuple",
                parent_ident,
                crate::utils::to_pascal_case_str(&field_ident.to_string())
            );

            let mut nested_extra = TokenStream::new();
            let mut tuple_fields = Vec::new();

            for (i, item) in tuple.items.iter().enumerate() {
                let item_tag = (i + 1) as u32;
                let item_name = format_ident!("item_{}", i);

                let rendered = render_field_with_helpers(&tuple_ident, &item_name, item_tag, item);

                nested_extra.extend(rendered.extra_defs);
                tuple_fields.push(rendered.field);
            }

            let extra_defs = quote! {
                #nested_extra
                #[derive(Clone, PartialEq, ::prost::Message, ::borsh::BorshDeserialize, ::borsh::BorshSerialize)]
                pub struct #tuple_ident {
                    #(#tuple_fields),*
                }
            };

            let field = quote! {
                #[prost(message, optional, tag = #tag)]
                pub #field_ident: ::core::option::Option<#tuple_ident>
            };

            ProstFieldTokens { extra_defs, field }
        },

        // Link => message field; must be Option<T> unless repeated/map.
        T::Link(_) => {
            let (attr, ty) = map_codama_type_to_prost(type_node);

            let field = quote! {
                #[prost(#attr, optional, tag = #tag)]
                pub #field_ident: ::core::option::Option<#ty>
            };

            ProstFieldTokens {
                extra_defs: quote! {},
                field,
            }
        },

        // default case: scalar/bytes/repeated/option already mapped.
        other => {
            let (attr, ty) = map_codama_type_to_prost(other);
            let field = quote! {
                #[prost(#attr, tag = #tag)]
                pub #field_ident: #ty
            };
            ProstFieldTokens {
                extra_defs: quote! {},
                field,
            }
        },
    }
}

/**
 * Returns:
 * - prost attribute tokens (e.g. `uint64`, `bytes = "vec"`)
 * - Rust field type tokens (e.g. `u64`, `Vec<u8>`)
 */
fn map_codama_type_to_prost(type_node: &codama_nodes::TypeNode) -> (TokenStream, TokenStream) {
    use codama_nodes::TypeNode as T;

    match type_node {
        T::String(_) | T::SizePrefix(_) => (quote!(string), quote!(::prost::alloc::string::String)),

        T::Bytes(_) => (quote!(bytes = "vec"), quote!(::prost::alloc::vec::Vec<u8>)),

        T::PublicKey(_) => (quote!(bytes = "vec"), quote!(PubkeyBytes)),

        T::Boolean(_) => (quote!(bool), quote!(bool)),

        T::Number(n) => match n.format {
            codama_nodes::NumberFormat::U8
            | codama_nodes::NumberFormat::U16
            | codama_nodes::NumberFormat::U32
            | codama_nodes::NumberFormat::ShortU16 => (quote!(uint32), quote!(u32)),

            codama_nodes::NumberFormat::U64 => (quote!(uint64), quote!(u64)),

            codama_nodes::NumberFormat::I8
            | codama_nodes::NumberFormat::I16
            | codama_nodes::NumberFormat::I32 => (quote!(int32), quote!(i32)),

            codama_nodes::NumberFormat::I64 => (quote!(int64), quote!(i64)),

            codama_nodes::NumberFormat::F32 => (quote!(float), quote!(f32)),
            codama_nodes::NumberFormat::F64 => (quote!(double), quote!(f64)),

            codama_nodes::NumberFormat::U128 | codama_nodes::NumberFormat::I128 => {
                (quote!(bytes = "vec"), quote!(::prost::alloc::vec::Vec<u8>))
            },
        },

        T::Option(o) => {
            let (inner_attr, inner_ty) = map_codama_type_to_prost(&o.item);
            (
                quote!(#inner_attr, optional),
                quote!(::core::option::Option<#inner_ty>),
            )
        },

        T::Array(a) => {
            let (inner_attr, inner_ty) = map_codama_type_to_prost(&a.item);
            (
                quote!(#inner_attr, repeated),
                quote!(::prost::alloc::vec::Vec<#inner_ty>),
            )
        },

        T::Link(link) => {
            let ident = format_ident!("{}", crate::utils::to_pascal_case(&link.name));
            (quote!(message), quote!(#ident))
        },

        T::FixedSize(node) => match *node.r#type {
            T::Bytes(_) | T::String(_) => {
                (quote!(bytes = "vec"), quote!(::prost::alloc::vec::Vec<u8>))
            },
            _ => todo!(),
        },

        _ => todo!("prost field type not implemented for {:?}", type_node),
    }
}
