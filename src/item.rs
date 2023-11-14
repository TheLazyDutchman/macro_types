use macro_types_helpers::{Into, ToTokens};
use proc_macro2::Span;
use quote::ToTokens;

use crate::{
	attr::Attr,
	expr::Constructor,
	generic::Generic,
	name::{Name, Path},
	tyref::TyRef,
	Field, ImplBlock,
};

pub trait HasGeneric {
	fn contains(&self, generic: &Generic) -> bool;
	fn associated_type_of(&self, generic: &Generic) -> Vec<Generic>;
}

pub struct Item<T> {
	pub value: NamedItem<T>,
	pub generics: Vec<Generic>,
}

impl<T> Item<T> {
	pub fn new(name: impl Into<Name>) -> Item<NoValue<T>> {
		Item {
			generics: Vec::new(),
			value: NamedItem::new(name),
		}
	}

	pub fn generic(&mut self, generic: impl Into<Generic>) -> &mut Self {
		self.generics
			.push(generic.into());
		self
	}

	pub fn map<U, F: FnOnce(T) -> U>(self, func: F) -> Item<U> {
		let Self { value, generics } = self;
		Item {
			generics,
			value: value.map(func),
		}
	}

	pub fn try_map<U, E, F: FnOnce(T) -> Result<U, E>>(self, func: F) -> Result<Item<U>, E> {
		let Self { value, generics } = self;
		Ok(Item {
			generics,
			value: value.try_map(func)?,
		})
	}

	pub fn ty_ref(&self) -> TyRef {
		let mut path: Path = self.name.clone().into();
		path.generics = self.generics.clone();
		path.into()
	}

	pub fn impl_block(&self) -> ImplBlock
	where
		T: HasGeneric,
	{
		let generics = self
			.generics
			.iter()
			.filter(|x| self.contains(x))
			.cloned()
			.chain(
				self.generics
					.iter()
					.flat_map(|x| self.associated_type_of(x)),
			);

		ImplBlock::new(self.ty_ref(), self.generics.clone(), generics.collect())
	}
}

impl Item<DeriveInput> {
	pub fn require_struct(self) -> Result<Item<Struct>, ()> {
		self.try_map(|x| x.try_into())
	}

	pub fn require_enum(self) -> Result<Item<Enum>, ()> {
		self.try_map(|x| x.try_into())
	}

	pub fn require_union(self) -> Result<Item<Enum>, ()> {
		self.try_map(|x| x.try_into())
	}
}

impl<T> std::ops::Deref for Item<T> {
	type Target = NamedItem<T>;

	fn deref(&self) -> &Self::Target {
		&self.value
	}
}

impl<T> std::ops::DerefMut for Item<T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.value
	}
}

pub struct NamedItem<T> {
	pub attrs: Vec<Attr>,
	pub name: Name,
	pub value: T,
}

impl<T> NamedItem<T> {
	pub fn new(name: impl Into<Name>) -> NamedItem<NoValue<T>> {
		NamedItem {
			attrs: Vec::new(),
			name: name.into(),
			value: NoValue(std::marker::PhantomData),
		}
	}

	pub fn span(&self) -> Span {
		self.name.span()
	}

	pub fn ty_ref(&self) -> TyRef {
		self.name.clone().into()
	}

	pub fn attr(&mut self, attr: impl Into<Attr>) -> &mut Self {
		self.attrs.push(attr.into());
		self
	}

	pub fn map<U, F: FnOnce(T) -> U>(self, func: F) -> NamedItem<U> {
		let Self { attrs, name, value } = self;
		NamedItem {
			attrs,
			name,
			value: func(value),
		}
	}

	pub fn try_map<U, E, F: FnOnce(T) -> Result<U, E>>(self, func: F) -> Result<NamedItem<U>, E> {
		let Self { attrs, name, value } = self;
		Ok(NamedItem {
			attrs,
			name,
			value: func(value)?,
		})
	}
}

impl<T> std::ops::Deref for NamedItem<T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		&self.value
	}
}

impl<T> std::ops::DerefMut for NamedItem<T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.value
	}
}

impl From<syn::DeriveInput> for Item<DeriveInput> {
	fn from(value: syn::DeriveInput) -> Self {
		let mut item = Item::new(value.ident);

		for attr in value.attrs {
			item.attr(attr);
		}

		for generic in value.generics.params {
			item.generic(generic);
		}

		item.value(value.data)
	}
}

pub struct NoValue<T>(std::marker::PhantomData<T>);

impl<T> Item<NoValue<T>> {
	fn value(self, value: impl Into<T>) -> Item<T> {
		let Self {
			value: old,
			generics,
		} = self;
		Item {
			value: old.value(value),
			generics,
		}
	}
}

impl<T> NamedItem<NoValue<T>> {
	fn value(self, value: impl Into<T>) -> NamedItem<T> {
		let Self { attrs, name, .. } = self;
		NamedItem {
			attrs,
			name,
			value: value.into(),
		}
	}
}

#[derive(Into, ToTokens)]
pub enum DeriveInput {
	Struct(Struct),
	Enum(Enum),

	#[to_tokens = "union"]
	Union,
}

impl From<syn::Data> for DeriveInput {
	fn from(value: syn::Data) -> Self {
		match value {
			syn::Data::Struct(value) => Self::Struct(
				value
					.fields
					.into_iter()
					.collect(),
			),
			syn::Data::Enum(value) => Self::Enum(
				value
					.variants
					.into_iter()
					.collect(),
			),
			syn::Data::Union(_) => Self::Union,
		}
	}
}

impl HasGeneric for DeriveInput {
	fn contains(&self, generic: &Generic) -> bool {
		match self {
			Self::Union => false,
			Self::Enum(value) => value.contains(generic),
			Self::Struct(value) => value.contains(generic),
		}
	}

	fn associated_type_of(&self, generic: &Generic) -> Vec<Generic> {
		match self {
			Self::Union => vec![],
			Self::Enum(value) => value.associated_type_of(generic),
			Self::Struct(value) => value.associated_type_of(generic),
		}
	}
}

pub struct Variant {
	pub fields: Fields,
}

impl NamedItem<Variant> {
	pub fn constructor(&self) -> Constructor {
		Constructor::new(self.ty_ref(), vec![])
	}
}

impl From<syn::Variant> for NamedItem<Variant> {
	fn from(value: syn::Variant) -> Self {
		let mut item = NamedItem::new(value.ident);

		for attr in value.attrs {
			item.attr(attr);
		}

		item.value(value.fields)
	}
}

impl From<syn::Fields> for Variant {
	fn from(value: syn::Fields) -> Self {
		Self {
			fields: Fields(
				value
					.into_iter()
					.map(|x| x.into())
					.collect(),
			),
		}
	}
}

impl ToTokens for NamedItem<Variant> {
	fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
		let name = &self.name;
		let fields = &self.fields;

		tokens.extend(quote::quote!(#name #fields));
	}
}

impl HasGeneric for Variant {
	fn contains(&self, generic: &Generic) -> bool {
		self.fields.contains(generic)
	}

	fn associated_type_of(&self, generic: &Generic) -> Vec<Generic> {
		self.fields
			.associated_type_of(generic)
	}
}

#[derive(ToTokens)]
#[to_tokens = "#(#variants),*"]
pub struct Enum {
	pub variants: Vec<NamedItem<Variant>>,
}

impl Enum {
	pub fn new(variants: Vec<NamedItem<Variant>>) -> Self {
		Self { variants }
	}
}

impl<I> FromIterator<I> for Enum
where
	NamedItem<Variant>: From<I>,
{
	fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
		Self::new(
			iter.into_iter()
				.map(|x| x.into())
				.collect(),
		)
	}
}

impl HasGeneric for Enum {
	fn contains(&self, generic: &Generic) -> bool {
		self.variants
			.iter()
			.any(|x| x.contains(generic))
	}

	fn associated_type_of(&self, generic: &Generic) -> Vec<Generic> {
		self.variants
			.iter()
			.flat_map(|x| x.associated_type_of(generic))
			.collect()
	}
}

#[derive(ToTokens)]
pub struct Struct {
	pub fields: Fields,
}

impl Struct {
	pub fn new(fields: impl Into<Fields>) -> Self {
		Self {
			fields: fields.into(),
		}
	}
}

impl<I> FromIterator<I> for Struct
where
	Field: From<I>,
{
	fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
		Self::new(Fields(
			iter.into_iter()
				.map(|x| x.into())
				.collect(),
		))
	}
}

impl HasGeneric for Struct {
	fn contains(&self, generic: &Generic) -> bool {
		self.fields.contains(generic)
	}

	fn associated_type_of(&self, generic: &Generic) -> Vec<Generic> {
		self.fields
			.associated_type_of(generic)
	}
}

impl NamedItem<Struct> {
	pub fn constructor(&self) -> Constructor {
		Constructor::new(self.ty_ref(), vec![])
	}
}

pub struct Fields(Vec<Field>);

impl From<Vec<Field>> for Fields {
	fn from(value: Vec<Field>) -> Self {
		Self(value)
	}
}

impl std::ops::Deref for Fields {
	type Target = Vec<Field>;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl std::ops::DerefMut for Fields {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

impl ToTokens for Fields {
	fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
		if self.is_empty() {
			return;
		}

		let fields = &self.0;

		if self[0].name.is_none() {
			tokens.extend(quote::quote!( ( #(#fields),* ) ));
		} else {
			tokens.extend(quote::quote!( { #(#fields),* } ));
		}
	}
}

impl HasGeneric for Fields {
	fn contains(&self, generic: &Generic) -> bool {
		self.0
			.iter()
			.any(|x| x.contains(generic))
	}

	fn associated_type_of(&self, generic: &Generic) -> Vec<Generic> {
		self.0
			.iter()
			.flat_map(|x| x.associated_type_of(generic))
			.collect()
	}
}
