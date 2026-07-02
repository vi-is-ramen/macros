use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Ident, Token, Block, Attribute};
use syn::parse::{Parse, ParseStream};

struct MacroArgs {
    version : Option<String>,
    kernel  : Option<String>,
    as_name : Option<String>,
}

impl Parse for MacroArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut args = MacroArgs { version: None, kernel: None, as_name: None };
        
        while !input.is_empty() {
            let name = if let Ok(ident) = input.parse::<Ident>() {
                ident.to_string()
            } else if input.peek(Token![as]) {
                input.parse::<Token![as]>()?;
                "as".to_string()
            } else {
                return Err(input.error("expected identifier or keyword"));
            };
            
            input.parse::<Token![=]>()?;
            let value: syn::LitStr = input.parse()?;
            
            match name.as_str() {
                "version" => args.version = Some(value.value()),
                "kernel" => args.kernel = Some(value.value()),
                "as" => args.as_name = Some(value.value()),
                _ => return Err(syn::Error::new(value.span(), "unknown attribute")),
            }
            
            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            }
        }
        
        Ok(args)
    }
}

struct ParsedFunc {
    attrs: Vec<Attribute>,
    vis: syn::Visibility,
    sig: syn::Signature,
    block: Option<Block>,
}

impl Parse for ParsedFunc {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis: syn::Visibility = input.parse()?;
        let sig: syn::Signature = input.parse()?;
        
        let block = if input.peek(syn::token::Brace) {
            let content;
            let brace = syn::braced!(content in input);
            Some(Block {
                brace_token: brace,
                stmts: content.call(Block::parse_within)?,
            })
        } else if input.peek(Token![;]) {
            input.parse::<Token![;]>()?;
            None
        } else {
            return Err(input.error("expected `{` or `;`"));
        };
        
        Ok(ParsedFunc { attrs, vis, sig, block })
    }
}

#[proc_macro_attribute]
pub fn import(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as MacroArgs);
    let func = parse_macro_input!(item as ParsedFunc);
    
    let prefix = if args.kernel.is_some() { "Ki" } else { "Mi" };
    let version_str = args.kernel.unwrap_or_else(|| args.version.expect("either version or kernel must be specified"));
    
    let fn_name = func.sig.ident.clone();
    let fn_vis = func.vis.clone();
    let fn_sig = func.sig.clone();
    
    let fn_attrs: Vec<_> = func.attrs.into_iter().filter(|attr| {
        !attr.path().is_ident("import") && !attr.path().is_ident("export")
    }).collect();
    
    let symbol_name = args.as_name.unwrap_or_else(|| fn_name.to_string());
    let export_name = format!("{}{}", prefix, symbol_name);
    let static_name = Ident::new(&format!("_{}", fn_name), fn_name.span());
    let stub_name = Ident::new(&format!("__stub_{}", fn_name), fn_name.span());
    
    let arg_names: Vec<_> = func.sig.inputs.iter().filter_map(|arg| {
        if let syn::FnArg::Typed(pat_type) = arg {
            if let syn::Pat::Ident(pat_ident) = &*pat_type.pat {
                Some(pat_ident.ident.clone())
            } else {
                None
            }
        } else {
            None
        }
    }).collect();

    let arg_types: Vec<_> = func.sig.inputs.iter().filter_map(|arg| {
        if let syn::FnArg::Typed(pat_type) = arg {
            let ty = &pat_type.ty;
            Some(quote! { #ty })
        } else {
            None
        }
    }).collect();

    let fn_ty = if func.sig.output == syn::ReturnType::Default {
        quote! { fn( #(#arg_types),* ) }
    } else {
        let ret = match &func.sig.output {
            syn::ReturnType::Type(_, ty) => quote! { #ty },
            _ => unreachable!()
        };
        quote! { fn( #(#arg_types),* ) -> #ret }
    };
    
    let mut wrapper_sig = fn_sig.clone();
    wrapper_sig.ident = fn_name.clone();
    wrapper_sig.output = func.sig.output.clone();
    
    let wrapper_func = quote! {
        #(#fn_attrs)*
        #[allow(non_snake_case)]
        #[inline(always)]
        #fn_vis #wrapper_sig {
            (unsafe{core::mem::transmute::<_, #fn_ty>(#static_name.0)})(#(#arg_names),*)
        }
    };
    
    let static_decl = if let Some(block) = func.block {
        let stub_func = quote! {
            #[allow(non_snake_case)]
            fn #stub_name #fn_sig { #block }
        };
        let static_var = quote! {
            #[used]
            #[allow(non_upper_case_globals)]
            #[unsafe(export_name = #export_name)]
            static #static_name: crate::ImExport = crate::ImExport(#stub_name as *const (), crate::parse_version(#version_str));
        };
        quote! {
            #stub_func
            #static_var
            #wrapper_func
        }
    } else {
        let static_var = quote! {
            #[used]
            #[allow(non_upper_case_globals)]
            #[unsafe(export_name = #export_name)]
            static #static_name: crate::ImExport = crate::ImExport(0 as *const (), crate::parse_version(#version_str));
        };
        quote! {
            #static_var
            #wrapper_func
        }
    };

    static_decl.into()
}

#[proc_macro_attribute]
pub fn export(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as MacroArgs);
    let func = parse_macro_input!(item as ParsedFunc);
    
    let prefix = if args.kernel.is_some() { "Ke" } else { "Me" };
    let version_str = args.kernel.clone().unwrap_or_else(|| args.version.expect("either version or kernel must be specified"));
    let is_kernel = args.kernel.is_some();
    
    let fn_name = func.sig.ident.clone();
    let fn_vis = func.vis.clone();
    let fn_sig = func.sig.clone();
    
    let fn_attrs: Vec<_> = func.attrs.into_iter().filter(|attr| {
        !attr.path().is_ident("import") && !attr.path().is_ident("export")
    }).collect();
    
    let symbol_name = args.as_name.clone().unwrap_or_else(|| fn_name.to_string());
    let export_name = format!("{}{}", prefix, symbol_name);
    
    let block = match func.block {
        Some(b) => b,
        None => return syn::Error::new(fn_name.span(), "exported functions must have a body").to_compile_error().into(),
    };
    
    let (wrapper_func, stub_func, ptr_ident, static_name) = if let Some(as_name) = args.as_name {
        let mut wrapper_sig = fn_sig.clone();
        wrapper_sig.ident = fn_name.clone();
        wrapper_sig.output = func.sig.output.clone();
        
        let wf = quote! {
            #(#fn_attrs)*
            #[allow(non_snake_case)]
            #fn_vis #wrapper_sig { #block }
        };
        let static_ident = Ident::new(&as_name, fn_name.span());
        (wf, quote! {}, quote! { #fn_name }, static_ident)
    } else {
        let stub_name = Ident::new(&format!("__stub_{}", fn_name), fn_name.span());
        let stub_sig = fn_sig.clone();
        
        let sf = quote! {
            #(#fn_attrs)*
            #[allow(non_snake_case)]
            fn #stub_name #stub_sig { #block }
        };
        (quote! {}, sf, quote! { #stub_name }, fn_name.clone())
    };
    
    let instance_name = Ident::new(&format!("{}_INSTANCE", static_name), static_name.span());

    let static_var = if is_kernel {
        quote! {
            #[used]
            #[allow(non_upper_case_globals)]
            #[unsafe(export_name = #export_name)]
            static #instance_name: crate::Kexport = crate::Kexport(
                #ptr_ident as *const (),
                crate::parse_version(#version_str),
                #symbol_name
            );

            #[linkme::distributed_slice(crate::KMI_TABLE)]
            fn #static_name() -> &'static crate::Kexport {
                &#instance_name
            }
        }
    } else {
        quote! {
            #[used]
            #[allow(non_upper_case_globals)]
            #[unsafe(export_name = #export_name)]
            static #instance_name: crate::ImExport = crate::ImExport(
                #ptr_ident as *const (),
                crate::parse_version(#version_str)
            );

            #[linkme::distributed_slice(crate::KMI_TABLE)]
            fn #static_name() -> &'static crate::ImExport {
                &#instance_name
            }
        }
    };
    
    quote! {
        #stub_func
        #wrapper_func
        #static_var
    }.into()
}
