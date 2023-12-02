use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{parse_macro_input, ItemFn};

fn extract_input_type(inputs: &syn::FnArg) -> String {
    match inputs {
        syn::FnArg::Receiver(_) => "self".to_string(),
        syn::FnArg::Typed(pat_type) => pat_type.ty.to_token_stream().to_string(),
    }
}

fn extract_output_type(inputs: &syn::ReturnType) -> String {
    match inputs {
        syn::ReturnType::Default => "void".to_string(),
        syn::ReturnType::Type(_, pat_type) => pat_type.to_token_stream().to_string(),
    }
}

#[proc_macro_attribute]
pub fn builtin(_: TokenStream, input: TokenStream) -> TokenStream {
    // Parse the input function
    let mut input_fn = parse_macro_input!(input as ItemFn);
    let fn_name = input_fn.sig.ident.clone();
    let var_name = format_ident!("{}__fn", &fn_name);

    // Check if the "compiler" feature flag is set
    if cfg!(feature = "compiler") {
        // Rename the function to __fn__function_name
        input_fn.sig.ident = var_name.clone();

        // Arg types as str
        let input_types = input_fn
            .sig
            .inputs
            .iter()
            .skip(1) // Skip over the builtin VM argument
            .filter_map(|arg| match arg {
                syn::FnArg::Typed(_) => Some(extract_input_type(arg).to_lowercase()),
                _ => None,
            })
            .collect::<Vec<String>>()
            .join(",");

        let output_type = extract_output_type(&input_fn.sig.output).to_string();

        // If the feature flag is set, generate the static variable
        let expanded = quote! {

            #input_fn

            pub const #fn_name: BuiltinInfo = BuiltinInfo {
                name: stringify!(#fn_name),
                inputs: #input_types,
                output: #output_type
            };

        };

        expanded.into()
    } else {
        // If the feature flag is not set, output the original function
        input_fn.into_token_stream().into()
    }
}
