use macro_types::{
	attr::{Attr, AttrValue},
	expr::{self, Block, Constructor, Expr, ExprValue, MatchVariant, UnpackExpr, Variable},
	generic::Generic,
	item::{self, DeriveInput, Enum, HasGeneric, Item, Struct, Trait},
	name::{Name, Path},
	tyref::{self, TyRef, TyRefValue, Unit, Wrapping},
	Arg, FunctionKind, ImplBlock,
};
use proc_macro::TokenStream;
use quote::ToTokens;
use syn::parse_macro_input;

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
	let input = parse_macro_input!(input as syn::DeriveInput);

	let object: Item<DeriveInput> = input.into();

	let span = object.span();

	let Ok(object): Result<Item<Enum>, _> = object.require_enum() else {
		return syn::Error::new(span, "Can only implement Into and TryInto for enums")
			.into_compile_error()
			.into();
	};

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
///     quote::quote!("Hello", " World")
/// );
/// assert_tokens_equal(
///     SequenceNamed { values: vec![69, 420] },
///     quote::quote!(69usize, 420usize)
/// );
/// # }
/// ```
#[proc_macro_derive(ToTokens, attributes(to_tokens))]
pub fn derive_to_tokens(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as syn::DeriveInput);

	let object: Item<item::DeriveInput> = input.into();

	let mut impl_block = object.impl_block();
	impl_block.for_trait(vec!["quote", "ToTokens"]);

	match &**object {
		item::DeriveInput::Struct(value) => {
			derive_to_tokens_struct(&mut impl_block, &object.attrs, value)
		}
		item::DeriveInput::Enum(value) => {
			derive_to_tokens_enum(&mut impl_block, object.name.clone(), value)
		}
		item::DeriveInput::Union => {
			return syn::Error::new(object.span(), "Cannot derive to_tokens for union")
				.to_compile_error()
				.into();
		}
	}

	quote::quote!(#impl_block).into()
}

fn derive_to_tokens_struct(impl_block: &mut ImplBlock, attrs: &[Attr], value: &Struct) {
	let mut exprs: Vec<Expr> = Vec::new();

	let mut field_names = Vec::new();

	for (index, field) in value
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
			&attrs,
			value
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
}

fn derive_to_tokens_enum(impl_block: &mut ImplBlock, name: Name, value: &Enum) {
	let mut match_expr = "self".match_expr(vec![]);

	for variant in &value.variants {
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
			vec![name.clone(), variant.name.clone()],
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
							// } else {
							// 	new_value += "#";
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

#[proc_macro_derive(Combinator)]
pub fn derive_combinator(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as syn::DeriveInput);

	let input: Item<DeriveInput> = input.into();

	let span = input.span();

	let Ok(input) = input.require_enum() else {
		return syn::Error::new(span, "Only an enum can be turned into a combinator.")
			.into_compile_error()
			.into();
	};

	let mut into_bound: Path = vec!["std", "convert", "Into"].into();
	into_bound
		.generics
		.push(Generic::Type(input.name.clone().into()));

	let mut trait_block: Item<Trait> = Item::new(
		input
			.name
			.map(|x| format!("{x}Combinator")),
	)
	.value(Trait::new(vec![], vec!["Sized".into(), into_bound.into()]));

	let gen_ty: TyRef = "T".into();

	let mut impl_block = ImplBlock::new(gen_ty.clone(), vec![gen_ty.into()], vec![]);

	impl_block.trait_bound(format!("T: std::convert::Into<{}>", input.name.clone()));

	impl_block.for_trait(trait_block.name.clone());

	for variant in &input.variants {
		if variant.fields.is_empty() {
			continue;
		}

		if !(variant.fields[0].contains(&Generic::Type(input.name.clone().into()))
			|| variant.fields[0].contains(&Generic::Type("Self".into())))
			|| !variant.fields[0]
				.ty
				.is_wrapped(Wrapping::Path(tyref::Path::new("Box")))
		{
			continue;
		}

		let mut constructor_expr = variant.constructor();
		// This should always be true
		let TyRef::Path(value) = &mut constructor_expr.owner else {
			unreachable!()
		};

		value.path = vec![input.name.clone(), variant.name.clone()].into();

		constructor_expr.add_field(
			variant.fields[0].name.clone(),
			vec!["std", "boxed", "Box", "new"].call(vec!["self"
				.field("into")
				.call(vec![])
				.into()]),
		);

		for (index, field) in variant
			.fields
			.iter()
			.skip(1)
			.enumerate()
		{
			constructor_expr.add_field(
				field.name.clone(),
				field
					.name
					.clone()
					.unwrap_or(format!("value{index}").into())
					.field("into")
					.call(vec![]),
			);
		}

		trait_block.function(
			variant.name.to_snake_case(),
			FunctionKind::Owned,
			variant
				.fields
				.iter()
				.skip(1)
				.enumerate()
				.map(|(index, x)| {
					Arg::new(
						x.name
							.clone()
							.unwrap_or(format!("value{index}").into()),
						x.ty.replace("Self", input.name.clone())
							.wrap(Wrapping::Path(tyref::Path::new(vec![
								"std", "convert", "Into",
							])))
							.impl_trait(),
					)
				})
				.collect(),
			Some(input.name.clone().into()),
			vec![constructor_expr.into()].into(),
		);
	}

	quote::quote!(#trait_block #impl_block).into()
}

#[proc_macro_derive(From, attributes(from))]
pub fn derive_from(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as syn::DeriveInput);

	let input: Item<DeriveInput> = input.into();
	let span = input.span();

	let Some(attr) = input
		.attrs
		.iter()
		.find(|x| x.name == "from".into())
	else {
		return syn::Error::new(span, "From derive macro requires an attribute to determine what type to convert from, like: `#[from(...names)]`").into_compile_error().into();
	};

	let AttrValue::Value(name) = &attr.value else {
		return syn::Error::new(
			attr.span(),
			r#"The from attr needs to be of the form `#[from = "{name}"]`, where name is the name from which this enum can be converted."#
		)
		.into_compile_error()
		.into();
	};

	let mut name = name.value.value();

	let is_exhaustive = if let Some(value) = name.strip_suffix("..") {
		name = value.to_string();
		true
	} else {
		false
	};

	let parts = name
		.split("::")
		.collect::<Vec<_>>();
	let name: Path = parts.into();

	let mut impl_block = input.impl_block();

	let mut path: Path = vec!["std", "convert", "From"].into();
	path.generics
		.push(Generic::Type(name.clone().into()));

	impl_block.for_trait(path);

	let expr = match &**input {
		DeriveInput::Struct(value) => {
			derive_from_struct(input.name.clone(), name.clone(), value, is_exhaustive)
		}
		DeriveInput::Enum(value) => derive_from_enum(input.name.clone(), name.clone(), value),
		DeriveInput::Union => {
			return syn::Error::new(span, "Cannot implement From for a union type.")
				.into_compile_error()
				.into()
		}
	};

	impl_block.function(
		"from",
		FunctionKind::Static,
		vec![Arg::new("value", name)],
		Some("Self"),
		vec![expr],
	);

	impl_block
		.into_token_stream()
		.into()
}

fn derive_from_struct(name: Name, from: Path, input: &Struct, is_exhaustive: bool) -> Expr {
	let mut constructor = Constructor::new(name, vec![]);
	let mut fields = Vec::new();

	for (index, field) in input
		.fields
		.iter()
		.enumerate()
	{
		let name = field
			.name
			.clone()
			.unwrap_or_else(|| format!("value{index}").into());

		let mut expr: Expr = name.clone().into();
		if let Some(attr) = field
			.attrs
			.iter()
			.find(|x| x.name == "from".into())
		{
			if let AttrValue::Value(value) = &attr.value {
				let value = value.value.value();
				let functions = value.split(",");

				for function in functions {
					expr = expr
						.field(function)
						.call(vec![])
						.into();
				}
			} else {
				expr = expr
					.field("into")
					.call(vec![])
					.into();
			}
		} else {
			expr = expr
				.field("into")
				.call(vec![])
				.into();
		}

		constructor.add_field(field.name.clone(), expr);
		fields.push(name);
	}

	let unpack = "value".unpack(UnpackExpr::new(from, fields, is_exhaustive));

	Block::new(vec![unpack.into(), constructor.into()]).into()
}

fn derive_from_enum(name: Name, from: Path, input: &Enum) -> Expr {
	let mut match_expr = "value".match_expr(vec![]);

	for variant in &input.variants {
		let mut from_name = from.clone();
		from_name
			.segments
			.push(variant.name.clone());

		let into_name = vec![name.clone(), variant.name.clone()];

		let mut fields = Vec::new();
		let mut constructor_expr = variant.constructor();
		constructor_expr.owner = into_name.into();

		for (index, field) in variant
			.fields
			.iter()
			.enumerate()
		{
			let name = field
				.name
				.clone()
				.unwrap_or_else(|| format!("value{index}").into());

			let mut expr: Expr = name.clone().into();
			if let Some(attr) = field
				.attrs
				.iter()
				.find(|x| x.name == "from".into())
			{
				if let AttrValue::Value(value) = &attr.value {
					let value = value.value.value();
					let functions = value.split(",");

					for function in functions {
						expr = expr
							.field(function)
							.call(vec![])
							.into();
					}
				} else {
					expr = expr
						.field("into")
						.call(vec![])
						.into();
				}
			} else {
				expr = expr
					.field("into")
					.call(vec![])
					.into();
			}

			constructor_expr.add_field(field.name.clone(), expr);

			fields.push(name);
		}

		match variant
			.attrs
			.iter()
			.find(|x| x.name == "from".into())
		{
			None => {
				match_expr.variant(
					from_name,
					fields,
					constructor_expr,
					variant.fields.is_empty()
						|| variant.fields[0]
							.name
							.is_none(),
					false,
				);
			}
			Some(attr) => {
				let AttrValue::List(values) = &attr.value else {
					return syn::Error::new(
						attr.span(),
						"from attribute has to be a list of values",
					)
					.to_compile_error()
					.into();
				};

				let Some(value) = values.values.first() else {
					return syn::Error::new(
						attr.span(),
						"attribute has to have at least a single value",
					)
					.to_compile_error()
					.into();
				};

				if value.name != "unwrap".into() {
					return syn::Error::new(
						attr.span(),
						"Currently, the only option supported here is `unwrap`.",
					)
					.to_compile_error()
					.into();
				}

				let mut path = value.value.value();

				let is_exhaustive = if let Some(value) = path.strip_suffix("..") {
					path = value.to_string();
					true
				} else {
					false
				};

				let parts = path
					.split("::")
					.collect::<Vec<_>>();
				let path: Path = parts.into();

				let block = Block::new(vec![
					"value"
						.unpack(UnpackExpr::new(path, fields, is_exhaustive))
						.into(),
					constructor_expr.into(),
				]);

				match_expr.variant(from_name, vec!["value".into()], block, true, false);
			}
		}
	}

	match_expr.into()
}
