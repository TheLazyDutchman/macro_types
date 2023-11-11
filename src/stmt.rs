use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

use crate::name::{Name, Path, TyRef};

#[derive(Debug, Clone)]
pub enum Expr {
	Constructor(Constructor),
	Call(Path, Vec<Expr>),
	CallMethod(Box<Expr>, Vec<Expr>),
	Unwrap(Box<Expr>),
	Field(Box<Expr>, Name),
	Variable(Path),
	Assign(Box<Expr>, Box<Expr>),
}

impl From<Constructor> for Expr {
	fn from(value: Constructor) -> Self {
		Self::Constructor(value)
	}
}

impl ToTokens for Expr {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		match self {
			Self::Constructor(value) => value.to_tokens(tokens),
			Self::Call(path, args) => tokens.extend(quote! { #path (#(#args),*) }),
			Self::CallMethod(method, args) => tokens.extend(quote! { #method (#(#args),*) }),
			Self::Unwrap(value) => tokens.extend(quote! { #value? }),
			Self::Field(value, name) => tokens.extend(quote! { #value.#name }),
			Self::Variable(name) => name.to_tokens(tokens),
			Self::Assign(to, from) => tokens.extend(quote! { #to = #from }),
		};
	}
}

#[derive(Debug, Clone)]
pub struct Constructor {
	pub tyref: TyRef,
	pub fields: Vec<FieldSetter>,
}

impl Constructor {
	pub fn new(tyref: impl Into<TyRef>) -> Self {
		Self {
			tyref: tyref.into(),
			fields: Vec::new(),
		}
	}

	pub fn field(&mut self, name: impl Into<Name>, expr: Expr) {
		self.fields
			.push(FieldSetter::new(name, expr))
	}
}

impl ToTokens for Constructor {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let tyref = &self.tyref;
		let fields = self.fields.iter().map(|x| {
			let name = &x.name;
			let expr = &x.expr;
			quote! { #name: #expr }
		});
		tokens.extend(quote! {
			#tyref { #(#fields),* }
		});
	}
}

#[derive(Debug, Clone)]
pub struct FieldSetter {
	pub name: Name,
	pub expr: Expr,
}

impl FieldSetter {
	pub fn new(name: impl Into<Name>, expr: Expr) -> Self {
		Self {
			name: name.into(),
			expr,
		}
	}
}
