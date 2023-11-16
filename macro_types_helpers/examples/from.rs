use macro_types::{generic::Generic, name::Name};
use macro_types_helpers::From;
use syn::{Attribute, Fields, FieldsNamed, Variant, WhereClause};

#[derive(From)]
#[from = "syn::Data"]
pub enum Data {
	#[from(unwrap = "syn::DataStruct..")]
	Struct { fields: Fields },
	#[from(unwrap = "syn::DataEnum..")]
	Enum {
		#[from = "into_iter,collect"]
		variants: Vec<Variant>,
	},
	#[from(unwrap = "syn::DataUnion..")]
	Union { fields: FieldsNamed },
}

#[derive(From)]
#[from = "syn::DeriveInput.."]
pub struct DeriveInput {
	pub attrs: Vec<Attribute>,
	pub ident: Name,
	pub generics: Generics,
	pub data: Data,
}

pub struct Generics {
	pub params: Vec<Generic>,
	pub where_clause: Option<WhereClause>,
}

impl From<syn::Generics> for Generics {
	fn from(value: syn::Generics) -> Self {
		let syn::Generics {
			params,
			where_clause,
			..
		} = value;
		Self {
			params: params
				.into_iter()
				.map(|x| x.into())
				.collect(),
			where_clause,
		}
	}
}

fn main() {}
