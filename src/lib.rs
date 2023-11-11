use attr::Attr;
use name::{Name, TyRef};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use stmt::{Constructor, Expr};
use syn::{Data, DeriveInput};

pub mod attr;
pub mod name;
pub mod stmt;

#[derive(Debug, Clone)]
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

	pub fn field(&mut self, name: Option<impl Into<Name>>, ty: impl Into<TyRef>) {
		self.fields
			.push(Field::new(name, ty, Vec::new()))
	}

	pub fn impl_block(&self) -> ImplBlock {
		ImplBlock::new(self.name.clone())
	}

	pub fn constructor(&self) -> Constructor {
		Constructor::new(self.name.clone())
	}

	pub fn method(
		&self,
		name: impl Into<Name>,
		kind: FunctionKind,
		args: Vec<Arg>,
		ret_ty: Option<TyRef>,
		block: Vec<Expr>,
	) -> ImplBlock {
		let mut impl_block = self.impl_block();
		impl_block.function(name, kind, args, ret_ty, block);
		impl_block
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

#[derive(Debug, Clone)]
pub struct Field {
	pub name: Option<Name>,
	pub ty: TyRef,
	pub attrs: Vec<Attr>,
}

impl Field {
	pub fn new(name: Option<impl Into<Name>>, ty: impl Into<TyRef>, attrs: Vec<Attr>) -> Field {
		Self {
			name: name.map(|x| x.into()),
			ty: ty.into(),
			attrs,
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
		Self::new(
			value.ident,
			value.ty,
			value
				.attrs
				.into_iter()
				.map(Attr::from)
				.collect(),
		)
	}
}

pub struct ImplBlock {
	tyref: TyRef,
	functions: Vec<Function>,
}

impl ImplBlock {
	pub fn new(tyref: impl Into<TyRef>) -> Self {
		Self {
			tyref: tyref.into(),
			functions: Vec::new(),
		}
	}

	pub fn function(
		&mut self,
		name: impl Into<Name>,
		kind: FunctionKind,
		args: Vec<Arg>,
		ret_ty: Option<TyRef>,
		block: Vec<Expr>,
	) {
		self.functions
			.push(Function::new(name, kind, args, ret_ty, block))
	}
}

impl ToTokens for ImplBlock {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let tyref = &self.tyref;
		let functions = &self.functions;

		tokens.extend(quote! {
			impl #tyref {
				#(#functions)*
			}
		})
	}
}

#[derive(Debug, Clone)]
pub struct Function {
	pub name: Name,
	pub kind: FunctionKind,
	pub args: Vec<Arg>,
	pub ret_ty: Option<TyRef>,
	pub block: Vec<Expr>,
}

impl Function {
	pub fn new(
		name: impl Into<Name>,
		kind: FunctionKind,
		args: Vec<Arg>,
		ret_ty: Option<TyRef>,
		block: Vec<Expr>,
	) -> Function {
		Self {
			name: name.into(),
			kind,
			args,
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
			FunctionKind::Static => None,
			FunctionKind::Owned => Some(quote!(self)),
			FunctionKind::MutOwned => Some(quote!(mut self)),
			FunctionKind::Ref => Some(quote!(&self)),
			FunctionKind::Mut => Some(quote!(&mut self)),
		};

		let mut args = self
			.args
			.iter()
			.map(|x| quote! {#x})
			.collect::<Vec<_>>();

		// If we do not return anything, add a semicolon after the last stmt
		let semi = if ret.is_none() { Some(quote!(;)) } else { None };

		if let Some(kind) = kind {
			args.insert(0, kind);
		}

		tokens.extend(quote! {
			pub fn #name(#(#args),*) #ret {
				#(#block);*

				#semi
			}
		})
	}
}

#[derive(Debug, Clone)]
pub enum FunctionKind {
	Static,
	Owned,
	MutOwned,
	Ref,
	Mut,
}

#[derive(Debug, Clone)]
pub struct Arg {
	name: Name,
	ty: TyRef,
}

impl Arg {
	pub fn new(name: impl Into<Name>, ty: impl Into<TyRef>) -> Arg {
		Self {
			name: name.into(),
			ty: ty.into(),
		}
	}
}

impl TryFrom<Field> for Arg {
	type Error = ();

	fn try_from(value: Field) -> Result<Self, Self::Error> {
		Ok(Self::new(value.name.ok_or(())?, value.ty))
	}
}

impl ToTokens for Arg {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let name = &self.name;
		let ty = &self.ty;

		tokens.extend(quote! {#name: #ty});
	}
}
