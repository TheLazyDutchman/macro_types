use quote::{quote, ToTokens};

use crate::{
	name::{Name, Path},
	tyref::TyRef,
};

crate::enum_definitions! {
	pub enum Expr {
		String { value: std::string::String } => { #value },
		Number { value: usize } => { #value },
		Variable { path: Path } => { #path },
		[value] Field { value: Box<Expr>, field: crate::name::Name } => { #value.#field },
		[value] Unwrap { value: Box<Expr> } => { #value? },
		[value] Assign { variable: Box<Expr>, value: Box<Expr> } => { #variable = #value },
		[value] Call { func: Box<Expr>, args: Vec<Expr> } => { #func(#(#args),*) },
		[value] CallMacro { func: Box<Expr>, args: Vec<Expr> } => { #func!(#(#args),*) },
		[value] Reference { value: Box<Expr> } => { &#value },
		Block { exprs: Vec<Expr> } => {{ #(#exprs);* }},
		Constructor { owner: TyRef, fields: Vec<ConstructorField> } => { #owner { #(#fields),* }}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstructorField {
	name: Name,
	expr: Expr,
}

impl ToTokens for ConstructorField {
	fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
		let name = &self.name;
		let expr = &self.expr;

		tokens.extend(quote! {
			#name: #expr
		})
	}
}

impl Constructor {
	pub fn add_field(&mut self, name: impl Into<Name>, expr: impl Into<Expr>) {
		self.fields
			.push(ConstructorField {
				name: name.into(),
				expr: expr.into(),
			})
	}
}

impl<T> ExprValue for T where Path: From<T> {}
impl<T> From<T> for Expr
where
	Path: From<T>,
{
	fn from(value: T) -> Self {
		Expr::Variable(Variable::new(value))
	}
}

impl<'a> From<&'a str> for String {
	fn from(value: &'a str) -> Self {
		value.to_string().into()
	}
}
