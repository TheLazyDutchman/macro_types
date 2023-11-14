use quote::{quote, ToTokens};

use crate::{
	name::{self, Name, Path},
	tyref::TyRef,
};

crate::enum_definitions! {
	pub enum Expr {
		Unit {} => { () },
		String { value: std::string::String } => { #value },
		Number { value: usize } => { #value },
		Variable { path: Path } => { #path },
		[value] Field { value: Box<Expr>, field: name::Name } => { #value.#field },
		[value] Index { value: Box<Expr>, field: syn::Index } => { #value.#field },
		[value] Unwrap { value: Box<Expr> } => { #value? },
		[value] PrependPound { value: Box<Expr> } => { ##value },
		[value] PrependDollar { value: Box<Expr> } => { $#value? },
		[value] Assign { variable: Box<Expr>, value: Box<Expr> } => { #variable = #value },
		[value] LetAssign { variable: Box<Expr>, value: Box<Expr> } => { let #variable = #value },
		[value] Call { func: Box<Expr>, args: Vec<Expr> } => { #func(#(#args),*) },
		[value] CallMacro { func: Box<Expr>, args: Vec<Expr> } => { #func!(#(#args),*) },
		[value] Reference { value: Box<Expr> } => { &#value },
		Block { exprs: Vec<Expr> } => {{ #(#exprs);* }},
		Constructor { owner: TyRef, fields: ConstructorFields } => { #owner #fields },
		[value] MatchExpr { value: Box<Expr>, variants: Vec<MatchVariant> } => { match #value { #(#variants),* } },
		TokenStream { value: Tokens } => { #value },
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstructorFields(Vec<ConstructorField>);

impl std::ops::Deref for ConstructorFields {
	type Target = Vec<ConstructorField>;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl std::ops::DerefMut for ConstructorFields {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

impl ToTokens for ConstructorFields {
	fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
		if self.is_empty() {
			return;
		}

		let fields = &self.0;

		if self[0].name.is_none() {
			tokens.extend(quote!( (#(#fields),*) ));
		} else {
			tokens.extend(quote!( {#(#fields),*} ));
		}
	}
}

impl From<Vec<ConstructorField>> for ConstructorFields {
	fn from(value: Vec<ConstructorField>) -> Self {
		Self(value)
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstructorField {
	name: Option<Name>,
	expr: Expr,
}

impl ToTokens for ConstructorField {
	fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
		let name = self
			.name
			.as_ref()
			.map(|x| quote!(#x:));
		let expr = &self.expr;

		tokens.extend(quote! {
			#name #expr
		})
	}
}

impl Constructor {
	pub fn add_field(&mut self, name: Option<impl Into<Name>>, expr: impl Into<Expr>) {
		self.fields
			.push(ConstructorField {
				name: name.map(|x| x.into()),
				expr: expr.into(),
			})
	}
}

impl MatchExpr {
	pub fn variant(
		&mut self,
		name: impl Into<Path>,
		fields: Vec<Name>,
		expr: impl Into<Expr>,
		is_tuple: bool,
		is_exhaustive: bool,
	) {
		self.variants
			.push(MatchVariant::new(
				name,
				fields,
				expr,
				is_tuple,
				is_exhaustive,
			));
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchVariant {
	pub name: Path,
	pub fields: Vec<Name>,
	pub expr: Expr,
	pub is_tuple: bool,
	is_exhaustive: bool,
}

impl MatchVariant {
	pub fn new(
		name: impl Into<Path>,
		fields: Vec<Name>,
		expr: impl Into<Expr>,
		is_tuple: bool,
		is_exhaustive: bool,
	) -> Self {
		Self {
			name: name.into(),
			fields,
			expr: expr.into(),
			is_tuple,
			is_exhaustive,
		}
	}
}

impl ToTokens for MatchVariant {
	fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
		let name = &self.name;

		let mut fields = self
			.fields
			.iter()
			.map(|x| quote!(#x))
			.collect::<Vec<_>>();

		if !self.is_exhaustive {
			fields.push(quote!(..));
		}

		let fields = if self.fields.is_empty() {
			None
		} else if self.is_tuple {
			Some(quote!( (#(#fields),*) ))
		} else {
			Some(quote!( {#(#fields),*} ))
		};

		let expr = &self.expr;

		tokens.extend(quote! {
			#name #fields => #expr
		});
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

impl<T> From<T> for Box<Expr>
where
	Path: From<T>,
{
	fn from(value: T) -> Self {
		Box::new(Expr::Variable(Variable::new(value)))
	}
}

impl<'a> From<&'a str> for String {
	fn from(value: &'a str) -> Self {
		value.to_string().into()
	}
}

#[derive(Debug, Clone)]
pub struct Tokens(proc_macro2::TokenStream);

impl PartialEq for Tokens {
	fn eq(&self, other: &Self) -> bool {
		self.0.to_string() == other.0.to_string()
	}
}

impl Eq for Tokens {}

impl ToTokens for Tokens {
	fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
		self.0.to_tokens(tokens)
	}
}

impl From<proc_macro2::TokenStream> for Expr {
	fn from(value: proc_macro2::TokenStream) -> Self {
		Self::TokenStream(Tokens(value).into())
	}
}

impl From<usize> for Expr {
	fn from(value: usize) -> Self {
		Expr::Number(value.into())
	}
}
