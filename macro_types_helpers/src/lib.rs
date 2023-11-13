use macro_types::{
	expr::{self, Expr, ExprValue, MatchVariant, Variable},
	name::Path,
	tyref::{TyRef, Unit},
	Arg, Enum, FunctionKind,
};
use proc_macro::TokenStream;
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
