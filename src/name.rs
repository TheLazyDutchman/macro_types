use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{GenericArgument, LitStr, PathArguments};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Name(Ident);

impl Name {
	pub fn map<S: AsRef<str>, F: FnOnce(String) -> S>(&self, func: F) -> Self {
		Ident::new(func(self.0.to_string()).as_ref(), self.0.span()).into()
	}

	pub fn inner(&self) -> &Ident {
		&self.0
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyRef {
	Path(Path),
	Ref(Box<TyRef>),
	RefMut(Box<TyRef>),
}

impl TyRef {
	pub fn wrap(&self, wrapping: Wrapping) -> Self {
		match wrapping {
			Wrapping::Path(segments) => Self::Path(Path {
				segments,
				generics: vec![Generic::TyRef(self.clone())],
			}),
			Wrapping::Ref => Self::Ref(Box::new(self.clone())),
			Wrapping::RefMut => Self::RefMut(Box::new(self.clone())),
		}
	}

	pub fn is_wrapped(&self, wrapping: Wrapping) -> bool {
		match wrapping {
			Wrapping::Path(segments) => {
				if let Self::Path(values) = self {
					if segments.len() == 1 && values.segments.len() > 1 {
						segments.last() == values.segments.last()
					} else {
						values.segments == segments
					}
				} else {
					false
				}
			}
			Wrapping::RefMut => self.is_ref_mut(),
			Wrapping::Ref => self.is_ref(),
		}
	}

	pub fn is_ref_mut(&self) -> bool {
		matches!(self, Self::RefMut(_))
	}

	pub fn is_ref(&self) -> bool {
		matches!(self, Self::Ref(_))
	}

	pub fn try_unwrap(&self, wrapping: Wrapping) -> Option<Self> {
		if !self.is_wrapped(wrapping) {
			return None;
		}

		match self {
			Self::Path(value) => Some(
				value
					.generics
					.first()
					.unwrap()
					.clone()
					.try_into()
					.unwrap(),
			),
			Self::RefMut(value) => Some(*value.clone()),
			Self::Ref(value) => Some(*value.clone()),
		}
	}
}

impl ToTokens for TyRef {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		match self {
			TyRef::Path(value) => value.to_tokens(tokens),
			TyRef::Ref(value) => tokens.extend(quote!(&#value)),
			TyRef::RefMut(value) => tokens.extend(quote!(&mut #value)),
		}
	}
}

impl<T> From<T> for TyRef
where
	Path: From<T>,
{
	fn from(value: T) -> Self {
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
			syn::Type::Reference(value) if value.mutability.is_none() => {
				Self::Ref(Box::new((*value.elem).into()))
			}
			syn::Type::Reference(value) if value.mutability.is_some() => {
				Self::RefMut(Box::new((*value.elem).into()))
			}
			syn::Type::Slice(_) => unimplemented!(),
			syn::Type::TraitObject(_) => unimplemented!(),
			syn::Type::Tuple(_) => unimplemented!(),
			syn::Type::Verbatim(_) => unimplemented!(),
			_ => unimplemented!(),
		}
	}
}

impl TryFrom<Generic> for TyRef {
	type Error = ();

	fn try_from(value: Generic) -> Result<Self, Self::Error> {
		match value {
			Generic::TyRef(value) => Ok(value),
		}
	}
}

pub enum Wrapping {
	Path(Vec<Name>),
	Ref,
	RefMut,
}
