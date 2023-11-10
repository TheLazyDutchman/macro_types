use name::Name;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{Data, DeriveInput};

mod name;

pub struct Struct {
	pub name: Name,
	pub fields: Vec<Field>,
}

impl Struct {
	pub fn new(name: impl Into<Name>) -> Self {
		Self {
			name: name.into(),
			fields: Vec::new(),
		}
	}

	pub fn field(&mut self, name: Option<impl Into<Name>>, ty: TokenStream) {
		self.fields
			.push(Field::new(name, ty))
	}

	pub fn method(
		&self,
		name: impl Into<Name>,
		kind: FunctionKind,
		ret_ty: Option<TokenStream>,
		block: TokenStream,
	) -> Method {
		Method::new(self.name.clone(), name, kind, ret_ty, block)
	}
}

impl ToTokens for Struct {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let name = &self.name;
		let fields = &self.fields;
		tokens.extend(quote! {
			pub struct #name {
				#(#fields),*
			}
		});
	}
}

impl TryFrom<DeriveInput> for Struct {
	type Error = ();

	fn try_from(value: DeriveInput) -> Result<Self, Self::Error> {
		let mut object = Struct::new(value.ident);

		let Data::Struct(value) = value.data else {
			return Err(());
		};

		for field in value.fields {
			object
				.fields
				.push(field.into());
		}

		Ok(object)
	}
}

pub struct Field {
	pub name: Option<Name>,
	pub ty: TokenStream,
}

impl Field {
	pub fn new(name: Option<impl Into<Name>>, ty: TokenStream) -> Field {
		Self {
			name: name.map(|x| x.into()),
			ty,
		}
	}
}

impl ToTokens for Field {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let name = &self.name;
		let ty = &self.ty;
		tokens.extend(quote! {
			#name: #ty
		});
	}
}

impl From<syn::Field> for Field {
	fn from(value: syn::Field) -> Self {
		Self::new(value.ident, value.ty.into_token_stream())
	}
}

pub struct Method {
	// TODO: This should be a typeref
	pub owner: Name,
	pub function: Function,
}

impl Method {
	pub fn new(
		owner: impl Into<Name>,
		name: impl Into<Name>,
		kind: FunctionKind,
		ret_ty: Option<TokenStream>,
		block: TokenStream,
	) -> Self {
		Self {
			owner: owner.into(),
			function: Function::new(name, kind, ret_ty, block),
		}
	}
}

impl ToTokens for Method {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let name = &self.owner;
		let function = &self.function;

		tokens.extend(quote! {
			impl #name {
				#function
			}
		});
	}
}

pub struct Function {
	pub name: Name,
	pub kind: FunctionKind,
	pub ret_ty: Option<TokenStream>,
	pub block: TokenStream,
}

impl Function {
	pub fn new(
		name: impl Into<Name>,
		kind: FunctionKind,
		ret_ty: Option<TokenStream>,
		block: TokenStream,
	) -> Function {
		Self {
			name: name.into(),
			kind,
			ret_ty,
			block,
		}
	}
}

impl ToTokens for Function {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let name = &self.name;
		let ret = self
			.ret_ty
			.as_ref()
			.map(|x| quote!(-> #x));
		let block = &self.block;

		let kind = match &self.kind {
			FunctionKind::Static => quote!(),
			FunctionKind::Ref => quote!(&self,),
		};

		tokens.extend(quote! {
			pub fn #name(#kind) #ret {
				#block
			}
		})
	}
}

pub enum FunctionKind {
	Static,
	Ref,
}
