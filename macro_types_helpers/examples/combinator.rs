use macro_types::{
	expr::{ConstructorFields, MatchVariant},
	name::{Name, Path},
	tyref::TyRef,
};
use macro_types_helpers::{Combinator, ToTokens};
use proc_macro2::TokenStream;
use quote::ToTokens;

#[derive(Combinator, ToTokens)]
pub enum Expr {
	#[to_tokens = "()"]
	Unit,
	String(String),
	Number(usize),
	Variable(Path),
	#[to_tokens = "#value.#field"]
	Field {
		value: Box<Self>,
		field: Name,
	},
	#[to_tokens = "#value.#index"]
	Index {
		value: Box<Self>,
		index: syn::Index,
	},
	#[to_tokens = "#0?"]
	Unwrap(Box<Self>),
	#[to_tokens = "##0"]
	PrependPound(Box<Self>),
	#[to_tokens = "$#0"]
	PrependDollar(Box<Self>),
	#[to_tokens = "#variable = #value"]
	Assign {
		variable: Box<Self>,
		value: Box<Self>,
	},
	#[to_tokens = "let #variable = #value"]
	LetAssign {
		variable: Box<Self>,
		value: Box<Self>,
	},
	#[to_tokens = "#func(#(#args),*)"]
	Call {
		func: Box<Self>,
		args: Vec<Self>,
	},
	#[to_tokens = "#func!(#(#args),*)"]
	CallMacro {
		func: Box<Self>,
		args: Vec<Self>,
	},
	#[to_tokens = "&#value"]
	Reference {
		value: Box<Self>,
	},
	#[to_tokens = "()"]
	Block(Vec<Self>),
	#[to_tokens = "#owner #fields"]
	Constructor {
		owner: TyRef,
		fields: ConstructorFields,
	},
	#[to_tokens = "match #value { #(#variants),* }"]
	MatchExpr {
		value: Box<Self>,
		variants: Vec<MatchVariant>,
	},
	TokensStream(TokenStream),
}

impl<'a> From<&'a str> for Expr {
	fn from(value: &'a str) -> Self {
		Expr::Variable(value.into())
	}
}

fn main() {
	let value = Expr::Unit;

	let value = value
		.field("Hello")
		.index(0)
		.assign("bonjour".unwrap());

	println!("{}", value.into_token_stream());
}
