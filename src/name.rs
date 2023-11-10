use proc_macro2::{Ident, Span};
use quote::{ToTokens, TokenStreamExt};

#[derive(Clone)]
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

impl ToTokens for Name {
	fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
		tokens.append(self.0.clone())
	}
}
