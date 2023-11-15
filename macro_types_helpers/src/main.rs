use macro_types_helpers::From;
use syn::{Fields, FieldsNamed, Variant};

#[derive(From)]
#[from = "syn::Data"]
pub enum DeriveInput {
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

fn main() {}
