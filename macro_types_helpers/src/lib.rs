use macro_types::{
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
///		Single(String),
///		SingleNamed { value: usize },
/// }
///
/// fn assert_to_tokens<T: quote::ToTokens>(value: T) {}
///
/// # fn main() {
/// assert_to_tokens(Value::Single("Hello, World!".to_string()));
///
/// let value = Value::Single("Hello, World".to_string());
/// assert_eq!(
/// quote::quote!(#value).to_string(),
/// quote::quote!("Hello, World").to_string()
/// )
/// # }
/// ```
#[proc_macro_derive(ToTokens)]
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

fn derive_to_tokens_struct(_object_struct: Struct) -> TokenStream {
	todo!()
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

		let variant_expr = "tokens"
			.field("extend")
			.call(vec![vec!["quote", "quote"]
				.call_macro(vec![field_names
					.first()
					.unwrap()
					.clone()
					.prepend_pound()
					.into()])
				.into()]);

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
