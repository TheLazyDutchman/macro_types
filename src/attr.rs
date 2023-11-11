use proc_macro2::Span;
use syn::{spanned::Spanned, LitStr, Meta, MetaList, MetaNameValue};

use crate::name::Path;

#[derive(Debug, Clone)]
pub struct Attr {
	pub name: Path,
	pub value: AttrValue,
	span: Option<Span>,
}

impl Attr {
	pub fn new(name: impl Into<Path>, value: impl Into<AttrValue>) -> Self {
		Self {
			name: name.into(),
			value: value.into(),
			span: None,
		}
	}

	pub fn span(&self) -> Span {
		self.span.unwrap().clone()
	}
}

impl From<syn::Attribute> for Attr {
	fn from(value: syn::Attribute) -> Self {
		let span = value.meta.span();
		let mut attr = Self::new(value.meta.path().clone(), value.meta);
		attr.span = Some(span);
		attr
	}
}

#[derive(Debug, Clone)]
pub enum AttrValue {
	None,
	List(List),
	Value(Value),
}

impl From<Meta> for AttrValue {
	fn from(value: Meta) -> Self {
		match value {
			Meta::Path(_) => Self::None,
			Meta::List(value) => Self::List(value.into()),
			Meta::NameValue(value) => Self::Value(value.into()),
		}
	}
}

#[derive(Debug, Clone)]
pub struct List {
	pub values: Vec<Value>,
}

impl From<MetaList> for List {
	fn from(value: MetaList) -> Self {
		let mut values = Vec::new();
		value
			.parse_nested_meta(|nested| {
				let name = nested.path.clone();
				let value = nested.value()?;
				let value: LitStr = value.parse()?;

				values.push(Value::new(name, value));
				Ok(())
			})
			.unwrap();
		Self { values }
	}
}

#[derive(Debug, Clone)]
pub struct Value {
	pub name: Path,
	pub value: LitStr,
}

impl Value {
	pub fn new(name: impl Into<Path>, value: impl Into<LitStr>) -> Self {
		Self {
			name: name.into(),
			value: value.into(),
		}
	}
}

impl From<MetaNameValue> for Value {
	fn from(value: MetaNameValue) -> Self {
		let name = value.path;
		let value = value.value;
		let syn::Expr::Lit(syn::ExprLit {
			lit: syn::Lit::Str(value),
			..
		}) = value
		else {
			panic!("We can't deal with anything that is not a string yet");
		};

		Self::new(name, value)
	}
}
