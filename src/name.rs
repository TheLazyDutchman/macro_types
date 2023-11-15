use convert_case::{Case, Casing};
use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{LitStr, PathArguments};

use crate::generic::Generic;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Name(Ident);

impl Name {
	pub fn map<S: AsRef<str>, F: FnOnce(String) -> S>(&self, func: F) -> Self {
		Ident::new(func(self.0.to_string()).as_ref(), self.0.span()).into()
	}

	pub fn to_snake_case(&self) -> Self {
		self.map(|x| x.to_case(Case::Snake))
	}

	pub fn span(&self) -> Span {
		self.0.span()
	}

	pub fn inner(&self) -> &Ident {
		&self.0
	}
}

impl std::fmt::Display for Name {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.0.fmt(f)
	}
}

impl<'a> From<&'a str> for Name {
	fn from(value: &'a str) -> Self {
		Ident::new(value, Span::call_site()).into()
	}
}

impl From<String> for Name {
	fn from(value: String) -> Self {
		(*value).into()
	}
}

impl From<Ident> for Name {
	fn from(value: Ident) -> Self {
		Self(value)
	}
}

impl From<LitStr> for Name {
	fn from(value: LitStr) -> Self {
		value.value().into()
	}
}

impl From<syn::PathSegment> for Name {
	fn from(value: syn::PathSegment) -> Self {
		value.ident.into()
	}
}

impl ToTokens for Name {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		tokens.append(self.0.clone())
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
	pub segments: Vec<Name>,
	pub generics: Vec<Generic>,
}

impl ToTokens for Path {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let segments = &self.segments;
		tokens.extend(quote! {#(#segments)::*});

		if !self.generics.is_empty() {
			let generics = &self.generics;
			tokens.extend(quote! {<#(#generics),*>});
		}
	}
}

impl From<syn::TypePath> for Path {
	fn from(value: syn::TypePath) -> Self {
		value.path.into()
	}
}

impl From<syn::Path> for Path {
	fn from(value: syn::Path) -> Self {
		let args = value
			.segments
			.last()
			.unwrap()
			.arguments
			.clone();
		let segments = value
			.segments
			.into_iter()
			.map(Name::from)
			.collect();
		let generics = match args {
			PathArguments::None => Vec::new(),
			PathArguments::Parenthesized(value) => value
				.inputs
				.into_iter()
				.map(Generic::from)
				.collect(),
			PathArguments::AngleBracketed(value) => value
				.args
				.into_iter()
				.map(Generic::from)
				.collect(),
		};

		Self { segments, generics }
	}
}

impl<T> From<Vec<T>> for Path
where
	Name: From<T>,
{
	fn from(value: Vec<T>) -> Self {
		Self {
			segments: value
				.into_iter()
				.map(Name::from)
				.collect(),
			generics: Vec::new(),
		}
	}
}

impl<T> From<T> for Path
where
	Name: From<T>,
{
	fn from(value: T) -> Self {
		vec![value].into()
	}
}
