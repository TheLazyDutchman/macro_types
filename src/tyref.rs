use crate::name::{self, Name};

crate::enum_definitions! {
	pub enum TyRef {
		Path { path: name::Path } => { #path },
		[value] RefTy { ty: Box<TyRef> } => { &#ty },
		[value] RefMut { ty: Box<TyRef> } => { &mut #ty },
	}
}

impl<T> TyRefValue for T where name::Path: From<T> {}

impl TyRef {
	pub fn wrapping(&self) -> Option<Wrapping> {
		match self {
			Self::Path(value) if value.generics.len() == 1 => Some(Wrapping::Path(value.clone())),
			Self::RefTy(_) => Some(Wrapping::Ref),
			Self::RefMut(_) => Some(Wrapping::RefMut),
			_ => None,
		}
	}

	pub fn is_wrapped(&self, wrapping: Wrapping) -> bool {
		self.wrapping() == Some(wrapping)
	}

	pub fn unwrap(&self) -> Result<Self, Self> {
		match self {
			Self::Path(value) if value.generics.len() == 1 => value
				.generics
				.first()
				.map(|x| match x {
					name::Generic::TyRef(value) => value.clone(),
				})
				.ok_or_else(|| self.clone()),
			Self::RefTy(value) => Ok(value.clone().into()),
			Self::RefMut(value) => Ok(value.clone().into()),
			_ => Err(self.clone()),
		}
	}

	pub fn wrap(&self, wrapping: Wrapping) -> Self {
		match wrapping {
			Wrapping::Path(mut value) => {
				value.generics = vec![self.clone().into()];
				Self::Path(value)
			}
			Wrapping::Ref => self.ref_ty().into(),
			Wrapping::RefMut => self.ref_mut().into(),
		}
	}
}

impl From<syn::Type> for TyRef {
	fn from(value: syn::Type) -> Self {
		match value {
			syn::Type::Array(_) => unimplemented!(),
			syn::Type::BareFn(_) => unimplemented!(),
			syn::Type::Group(_) => unimplemented!(),
			syn::Type::ImplTrait(_) => unimplemented!(),
			syn::Type::Infer(_) => unimplemented!(),
			syn::Type::Macro(_) => unimplemented!(),
			syn::Type::Never(_) => unimplemented!(),
			syn::Type::Paren(_) => unimplemented!(),
			syn::Type::Path(value) => Self::Path(Path::new(value)),
			syn::Type::Ptr(_) => unimplemented!(),
			syn::Type::Reference(value) if value.mutability.is_none() => {
				Self::RefTy(RefTy::new(*value.elem))
			}
			syn::Type::Reference(value) if value.mutability.is_some() => {
				Self::RefMut(RefMut::new(*value.elem))
			}
			syn::Type::Slice(_) => unimplemented!(),
			syn::Type::TraitObject(_) => unimplemented!(),
			syn::Type::Tuple(_) => unimplemented!(),
			syn::Type::Verbatim(_) => unimplemented!(),
			_ => unimplemented!(),
		}
	}
}

impl From<syn::Type> for Box<TyRef> {
	fn from(value: syn::Type) -> Self {
		Box::new(value.into())
	}
}

impl<T> From<T> for TyRef
where
	name::Path: From<T>,
{
	fn from(value: T) -> Self {
		Self::Path(Path::from(name::Path::from(value)))
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Wrapping {
	Path(Path),
	Ref,
	RefMut,
}
