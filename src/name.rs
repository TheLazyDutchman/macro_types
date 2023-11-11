use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{GenericArgument, PathArguments};

#[derive(Debug, Clone)]
pub struct Name(Ident);

impl Name {
	pub fn map<S: AsRef<str>, F: FnOnce(String) -> S>(&self, func: F) -> Self {
		Ident::new(func(self.0.to_string()).as_ref(), self.0.span()).into()
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

#[derive(Debug, Clone)]
pub struct Path {
	segments: Vec<Name>,
	generics: Vec<Generic>,
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

#[derive(Debug, Clone)]
pub enum Generic {
	TyRef(TyRef),
}

impl ToTokens for Generic {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		match self {
			Generic::TyRef(value) => value.to_tokens(tokens),
		}
	}
}

impl From<GenericArgument> for Generic {
	fn from(value: GenericArgument) -> Self {
		match value {
			GenericArgument::Lifetime(_) => unimplemented!(),
			GenericArgument::Type(value) => Self::TyRef(value.into()),
			GenericArgument::Const(_) => unimplemented!(),
			GenericArgument::AssocType(_) => unimplemented!(),
			GenericArgument::AssocConst(_) => unimplemented!(),
			GenericArgument::Constraint(_) => unimplemented!(),
			_ => unimplemented!(),
		}
	}
}

impl From<syn::Type> for Generic {
	fn from(value: syn::Type) -> Self {
		Self::TyRef(value.into())
	}
}

#[derive(Debug, Clone)]
pub enum TyRef {
	Path(Path),
}

impl TyRef {
	pub fn wrap(&self, wrapping: Wrapping) -> Self {
		match wrapping {
			Wrapping::Path(segments) => Self::Path(Path {
				segments,
				generics: vec![Generic::TyRef(self.clone())],
			}),
		}
	}
}

impl ToTokens for TyRef {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		match self {
			TyRef::Path(value) => value.to_tokens(tokens),
		}
	}
}

impl From<Path> for TyRef {
	fn from(value: Path) -> Self {
		Self::Path(value)
	}
}

impl From<Name> for TyRef {
	fn from(value: Name) -> Self {
		Self::Path(value.into())
	}
}

impl From<syn::Type> for TyRef {
	fn from(value: syn::Type) -> Self {
		match value {
			syn::Type::Array(_) => unimplemented!(),
			syn::Type::BareFn(_) => unimplemented!(),
			syn::Type::Group(_) => unimplemented!(),
			syn::Type::ImplTrait(_) => unimplemented!(),
			syn::Type::Infer(_) => unimplemented!(),
			syn::Type::Macro(_) => unimplemented!(),
			syn::Type::Never(_) => unimplemented!(),
			syn::Type::Paren(_) => unimplemented!(),
			syn::Type::Path(value) => Self::Path(value.into()),
			syn::Type::Ptr(_) => unimplemented!(),
			syn::Type::Reference(_) => unimplemented!(),
			syn::Type::Slice(_) => unimplemented!(),
			syn::Type::TraitObject(_) => unimplemented!(),
			syn::Type::Tuple(_) => unimplemented!(),
			syn::Type::Verbatim(_) => unimplemented!(),
			_ => unimplemented!(),
		}
	}
}

pub enum Wrapping {
	Path(Vec<Name>),
}
