use quote::ToTokens;
use syn::GenericArgument;

use crate::tyref::TyRef;

crate::enum_definitions! {
	pub enum Generic {
		Type { ty_ref: TyRef, bounds: Bounds } => { #ty_ref #bounds },
		LifeTime { _lt: Lt } => { },
	}
}

impl From<TyRef> for Generic {
	fn from(value: TyRef) -> Self {
		Self::Type(Type::new(value, vec![]))
	}
}

impl<T> From<T> for Type
where
	TyRef: From<T>,
{
	fn from(value: T) -> Self {
		Self::new(value, vec![])
	}
}

impl Generic {
	pub fn contains(&self, other: &Generic) -> bool {
		match self {
			Self::Type(value) => value.ty_ref.contains(other),
			_ => unimplemented!(),
		}
	}

	pub fn associated_type_of(&self, other: &Generic) -> Vec<Generic> {
		match self {
			Self::Type(value) => value
				.ty_ref
				.associated_type_of(other),
			_ => unimplemented!(),
		}
	}

	pub fn without_bounds(&self) -> Generic {
		match self.clone() {
			Self::Type(mut value) => {
				value.bounds.clear();
				Self::Type(value)
			}
			_ => unimplemented!(),
		}
	}
}

impl From<GenericArgument> for Generic {
	fn from(value: GenericArgument) -> Self {
		match value {
			GenericArgument::Lifetime(_) => unimplemented!(),
			GenericArgument::Type(value) => Self::Type(value.into()),
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
		Self::Type(value.into())
	}
}

impl From<syn::GenericParam> for Generic {
	fn from(value: syn::GenericParam) -> Self {
		match value {
			syn::GenericParam::Type(value) => Self::Type(Type::new(value.ident, value.bounds)),
			_ => unimplemented!(),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Bounds(Vec<TyRef>);

impl From<Vec<TyRef>> for Bounds {
	fn from(value: Vec<TyRef>) -> Self {
		Self(value)
	}
}

impl std::ops::Deref for Bounds {
	type Target = Vec<TyRef>;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl std::ops::DerefMut for Bounds {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

impl ToTokens for Bounds {
	fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
		if self.len() == 0 {
			return;
		}

		let bounds = &self.0;

		tokens.extend(quote::quote!(: #(#bounds),*));
	}
}

impl From<syn::punctuated::Punctuated<syn::TypeParamBound, syn::token::Plus>> for Bounds {
	fn from(value: syn::punctuated::Punctuated<syn::TypeParamBound, syn::token::Plus>) -> Self {
		let mut bounds = vec![];
		for value in value {
			bounds.push(value.into());
		}
		Self(bounds)
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lt;
