use macro_types::{
	attr::{Attr, AttrValue},
	expr::{self, Expr, ExprValue, MatchVariant, Variable},
	name::Path,
	tyref::{TyRef, TyRefValue, Unit},
	Arg, Enum, FunctionKind, Struct,
};
use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{parse_macro_input, DeriveInput};

/// Derives [`From`] and [`TryInto`] for all unit
/// variants
///
/// ```rust
/// use macro_types_helpers::Into;
///
/// #[derive(Debug, Into, PartialEq, Eq)]
/// enum A {
///     Number(usize),
///     String(String)
/// }
///
/// # fn main() {
/// assert_eq!(
/// A::String("Hello, World!".to_string()),
/// "Hello, World!".to_string().into()
/// );
/// assert_eq!(
/// Ok(69),
/// A::Number(69).try_into()
/// );
/// assert_eq!(
/// Err::<usize, ()>(()),
/// A::String("Hello, World!".to_string()).try_into()
/// );
/// # }
/// ```
#[proc_macro_derive(Into)]
pub fn derive_into(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as DeriveInput);

	let object: Enum = input.try_into().unwrap();

	let mut stream = proc_macro2::TokenStream::new();

	for variant in &object.variants {
		let mut impl_block = object.impl_block();

		if variant.fields.len() != 1 {
			continue;
		}

		let field = variant
			.fields
			.first()
			.unwrap()
			.clone();

		let mut path: Path = vec!["std", "convert", "From"].into();
		path.generics = vec![field.ty.clone().into()];
		impl_block.for_trait(path);

		let mut constructor = variant.constructor();
		constructor.owner = vec![object.name.clone(), variant.name.clone()].into();

		constructor.add_field(field.name.clone(), Expr::Variable(Variable::new("value")));

		impl_block.function(
			"from",
			FunctionKind::Static,
			vec![Arg::new("value", field.ty.clone())],
			Some("Self"),
			vec![constructor.into()],
		);

		stream.extend(quote::quote! {
			#impl_block
		});

		let mut impl_block = object.impl_block();

		let mut path: Path = vec!["std", "convert", "TryInto"].into();
		path.generics = vec![field.ty.clone().into()];
		impl_block.for_trait(path);

		let field_name = field
			.name
			.clone()
			.unwrap_or("value".into());

		let mut match_expr = "self".match_expr(vec![MatchVariant::new(
			vec![object.name.clone(), variant.name.clone()],
			vec![field_name.clone()],
			vec!["std", "result", "Result", "Ok"].call(vec![field_name.into()]),
			field.name.is_none(),
			false,
		)]);

		if object.variants.len() > 1 {
			match_expr.variant(
				"_",
				vec![],
				vec!["std", "result", "Result", "Err"].call(vec![Expr::Unit(expr::Unit::new())]),
				false,
				false,
			);
		}

		impl_block.associated_type("Error", TyRef::Unit(Unit::new()));

		let mut ret_ty: Path = vec!["std", "result", "Result"].into();
		ret_ty
			.generics
			.push(field.ty.into());
		ret_ty
			.generics
			.push(TyRef::from(vec!["Self", "Error"]).into());

		impl_block.function(
			"try_into",
			FunctionKind::Owned,
			vec![],
			Some(ret_ty),
			vec![match_expr.into()],
		);

		stream.extend(quote::quote! {
			#impl_block
		});
	}

	stream.into()
}

/// Derives the [`ToTokens`] trait for types
///
/// ```rust
/// use macro_types_helpers::ToTokens;
///
/// #[derive(ToTokens)]
/// enum Value {
///		#[to_tokens = "$"]
///		Unit,
///
///		Single(String),
///
///		SingleNamed { value: usize },
///
///		#[to_tokens = "Test {
///			first: #0,
///			second: #1
///		}"]
///		Multiple(String,usize),
///
///		#[to_tokens = "Test {
///			first: #first,
///			second: #second
///		}"]
///		MultipleNamed { first: String, second: usize },
/// }
///
/// fn assert_tokens_equal<T: quote::ToTokens, U: quote::ToTokens>(left: T, right: U) {
/// 	assert_eq!(
/// 		quote::quote!(#left).to_string(),
/// 		quote::quote!(#right).to_string()
/// 	)
/// }
/// # fn main() {
/// assert_tokens_equal(Value::Unit, quote::quote!($));
/// assert_tokens_equal(Value::Single("Hello, World".to_string()), "Hello, World".to_string());
/// assert_tokens_equal(Value::SingleNamed { value: 69 }, 69usize);
/// assert_tokens_equal(
/// 	Value::Multiple("Hello, World".to_string(),69),
/// 	quote::quote!(Test {
/// 		first: "Hello, World",
/// 		second: 69usize
/// 	})
/// );
/// assert_tokens_equal(
/// 	Value::MultipleNamed { first: "Hello, World".to_string(), second: 69 },
/// 	quote::quote!(Test {
/// 		first: "Hello, World",
/// 		second: 69usize
/// 	})
/// );
/// # }
/// ```
///
/// It also works for structs
/// ```rust
/// use macro_types_helpers::ToTokens;
///
/// #[derive(ToTokens)]
/// #[to_tokens = "$"]
/// struct Unit;
///
/// #[derive(ToTokens)]
/// struct Single(String);
///
/// #[derive(ToTokens)]
/// struct SingleNamed {
/// 	value: usize
/// }
///
/// #[derive(ToTokens)]
///	#[to_tokens = "Test {
///		first: #0,
///		second: #1
///	}"]
/// struct Multiple(String, usize);
///
/// #[derive(ToTokens)]
///	#[to_tokens = "Test {
///		first: #first,
///		second: #second
///	}"]
/// struct MultipleNamed {
/// 	first: String,
/// 	second: usize
/// }
///
/// #[derive(ToTokens)]
/// #[to_tokens = "#(#0),*"]
/// struct Sequence(Vec<String>);
///
/// #[derive(ToTokens)]
/// #[to_tokens = "#(#values),*"]
/// struct SequenceNamed{ values: Vec<usize> }
///
/// fn assert_tokens_equal<T: quote::ToTokens, U: quote::ToTokens>(left: T, right: U) {
/// 	assert_eq!(
/// 		quote::quote!(#left).to_string(),
/// 		quote::quote!(#right).to_string()
/// 	)
/// }
/// # fn main() {
/// assert_tokens_equal(Unit, quote::quote!($));
/// assert_tokens_equal(Single("Hello, World".to_string()), "Hello, World".to_string());
/// assert_tokens_equal(SingleNamed { value: 69 }, 69usize);
/// assert_tokens_equal(
/// 	Multiple("Hello, World".to_string(),69),
/// 	quote::quote!(Test {
/// 		first: "Hello, World",
/// 		second: 69usize
/// 	})
/// );
/// assert_tokens_equal(
/// 	MultipleNamed { first: "Hello, World".to_string(), second: 69 },
/// 	quote::quote!(Test {
/// 		first: "Hello, World",
/// 		second: 69usize
/// 	})
/// );
/// assert_tokens_equal(
///     Sequence(vec!["Hello".to_string(), " World".to_string()]),
///     quote::quote!("Hello", "World")
/// );
/// assert_tokens_equal(
///     SequenceNamed { values: vec![69, 420] },
///     quote::quote!(69usize, 420usize)
/// );
/// # }
/// ```
#[proc_macro_derive(ToTokens, attributes(to_tokens))]
pub fn derive_to_tokens(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as DeriveInput);

	let object_enum: Result<Enum, _> = input.clone().try_into();
	let object_struct: Result<Struct, _> = input.try_into();

	if let Ok(object_enum) = object_enum {
		derive_to_tokens_enum(object_enum)
	} else if let Ok(object_struct) = object_struct {
		derive_to_tokens_struct(object_struct)
	} else {
		// TODO: Add errors
		TokenStream::new()
	}
}

fn derive_to_tokens_struct(object: Struct) -> TokenStream {
	let mut impl_block = object.impl_block();

	impl_block.for_trait(vec!["quote", "ToTokens"]);

	let mut exprs: Vec<Expr> = Vec::new();

	let mut field_names = Vec::new();

	for (index, field) in object
		.fields
		.iter()
		.enumerate()
	{
		let mut field_expr: Expr = "self".into();

		if let Some(name) = field.name.clone() {
			field_expr = field_expr.field(name).into();
		} else {
			field_expr = field_expr.index(index).into();
		}

		let variable_name = field
			.name
			.clone()
			.unwrap_or(format!("value{index}").into());

		exprs.push(
			variable_name
				.clone()
				.let_assign(field_expr.reference())
				.into(),
		);

		field_names.push(variable_name);
	}

	exprs.push(
		fields_to_tokens_expr(
			&field_names,
			&object.attrs,
			object
				.fields
				.first()
				.is_some_and(|x| x.name.is_none()),
		)
		.unwrap(),
	);

	let ret_ty: Option<TyRef> = None;
	impl_block.function(
		"to_tokens",
		FunctionKind::Ref,
		vec![Arg::new(
			"tokens",
			vec!["proc_macro2", "TokenStream"].ref_mut(),
		)],
		ret_ty,
		exprs,
	);

	impl_block
		.into_token_stream()
		.into()
}

fn derive_to_tokens_enum(object: Enum) -> TokenStream {
	let mut impl_block = object.impl_block();

	impl_block.for_trait(vec!["quote", "ToTokens"]);

	let mut match_expr = "self".match_expr(vec![]);

	for variant in object.variants {
		let is_tuple = variant.fields.is_empty()
			|| variant.fields[0]
				.name
				.is_none();

		let field_names = variant
			.fields
			.iter()
			.enumerate()
			.map(|(index, field)| {
				field
					.name
					.clone()
					.unwrap_or(format!("value{index}").into())
			})
			.collect::<Vec<_>>();

		let variant_expr = fields_to_tokens_expr(
			&field_names,
			&variant.attrs,
			variant
				.fields
				.first()
				.is_some_and(|x| x.name.is_none()),
		)
		.unwrap();

		match_expr.variant(
			vec![object.name.clone(), variant.name],
			field_names,
			variant_expr,
			is_tuple,
			false,
		);
	}

	let ret_ty: Option<TyRef> = None;
	impl_block.function(
		"to_tokens",
		FunctionKind::Ref,
		vec![Arg::new(
			"tokens",
			vec!["proc_macro2", "TokenStream"].ref_mut(),
		)],
		ret_ty,
		vec![match_expr.into()],
	);

	impl_block
		.into_token_stream()
		.into()
}

fn fields_to_tokens_expr(
	names: &[impl Into<Expr> + Clone],
	attrs: &[Attr],
	is_tuple: bool,
) -> Result<Expr, &'static str> {
	let expr: Expr = if let Some(attr) = attrs
		.iter()
		.find(|x| x.name == "to_tokens".into())
	{
		if let AttrValue::Value(value) = &attr.value {
			let mut value = value.value.value();

			if is_tuple {
				eprintln!("{value}");
				let mut new_value = String::new();
				if value.starts_with('#') {
					new_value.push('#');
				}

				for part in value.split('#') {
					if new_value.is_empty() {
						new_value += part;
						continue;
					}

					if let Some(part) = part.chars().next() {
						if part.is_numeric() {
							new_value += "#value";
						} else {
							new_value += "#";
						}
					}

					new_value += part;
				}
				eprintln!("{new_value}");

				value = new_value;
			}

			let value: proc_macro2::TokenStream = value.parse().unwrap();
			value.into()
		} else {
			return Err(r#"to_tokens helper macro should be a name value pair"#);
		}
	} else if names.len() == 1 {
		names[0]
			.clone()
			.into()
			.prepend_pound()
			.into()
	} else {
		return Err(
			r#"If you don't have fields, or have more than one, you need to use #[to_tokens "..."]"#,
		);
	};

	Ok("tokens"
		.field("extend")
		.call(vec![vec!["quote", "quote"]
			.call_macro(vec![expr])
			.into()])
		.into())
}
