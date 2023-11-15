use crate::{
	generic::{self, Generic},
	name::{self, Name},
};

crate::enum_definitions! {
	pub enum TyRef {
		Unit { } => { () },
		Path { path: name::Path } => { #path },
		[value] RefTy { ty: Box<TyRef> } => { &#ty },
		[value] RefMut { ty: Box<TyRef> } => { &mut #ty },
		[value] ImplTrait { value: Box<TyRef> } => { impl #value },
	}
}

impl<T> TyRefValue for T where name::Path: From<T> {}

impl TyRef {
	pub fn wrapping(&self) -> Option<Wrapping> {
		match self {
			Self::Path(value) if value.generics.len() == 1 => {
				let mut value = value.clone();
				value.generics.clear();

				Some(Wrapping::Path(value))
			}
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
					Generic::Type(value) => value.ty_ref.clone(),
					_ => unimplemented!(),
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

	pub fn contains(&self, other: &Generic) -> bool {
		match self {
			Self::Unit(_) => false,
			Self::Path(value)
				if value
					.path
					.segments
					.last()
					.cloned() != Some("PhantomData".into()) =>
			{
				if let Generic::Type(generic::Type {
					ty_ref: Self::Path(other),
					..
				}) = other
				{
					if value.path == other.path {
						return true;
					}
				}

				for generic in &value.generics {
					if generic.contains(other) {
						return true;
					}
				}

				false
			}
			Self::Path(_) => false,
			Self::RefTy(value) => value.contains(other),
			Self::RefMut(value) => value.contains(other),
			Self::ImplTrait(value) => value.contains(other),
		}
	}

	pub fn associated_type_of(&self, other: &Generic) -> Vec<Generic> {
		match self {
			Self::Unit(_) => vec![],
			Self::Path(value)
				if value
					.path
					.segments
					.last()
					.cloned() != Some("PhantomData".into()) =>
			{
				let Generic::Type(generic::Type {
					ty_ref: Self::Path(path),
					..
				}) = other
				else {
					return Vec::new();
				};

				let mut generics: Vec<Generic> = value
					.generics
					.iter()
					.map(|x| x.associated_type_of(other))
					.flatten()
					.collect();

				if value.path.segments.first() == path.segments.first() {
					generics.push(Generic::Type(self.clone().into()));
				}
				generics
			}
			Self::Path(_) => Vec::new(),
			Self::RefTy(value) => value.associated_type_of(other),
			Self::RefMut(value) => value.associated_type_of(other),
			Self::ImplTrait(value) => value.associated_type_of(other),
		}
	}

	pub fn without_bounds(&self) -> Self {
		match self.clone() {
			Self::Unit(value) => Self::Unit(value),
			Self::Path(mut value) => {
				for generic in &mut value.generics {
					*generic = generic.without_bounds();
				}

				Self::Path(value)
			}
			TyRef::RefTy(value) => value
				.without_bounds()
				.ref_ty()
				.into(),
			TyRef::RefMut(value) => value
				.without_bounds()
				.ref_mut()
				.into(),
			TyRef::ImplTrait(value) => value
				.without_bounds()
				.impl_trait()
				.into(),
		}
	}

	pub fn replace(&self, reference: impl Into<Name>, other: impl Into<Name>) -> Self {
		let reference = reference.into();
		let other = other.into();

		let mut value = self.clone();
		match value {
			Self::Path(ref mut value) => {
				if value.segments.len() == 1 && value.segments[0] == reference {
					value.segments = vec![other.clone()];
				}

				for generic in &mut value.generics {
					*generic = generic.replace(reference.clone(), other.clone());
				}
			}
			Self::RefTy(ref mut inner) => {
				value = inner
					.replace(reference, other)
					.ref_ty()
					.into()
			}
			Self::RefMut(ref mut inner) => {
				value = inner
					.replace(reference, other)
					.ref_mut()
					.into()
			}
			Self::Unit(_) => {}
			Self::ImplTrait(inner) => {
				value = inner
					.replace(reference, other)
					.impl_trait()
					.into()
			}
		};

		value
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

impl From<syn::TypeParamBound> for TyRef {
	fn from(value: syn::TypeParamBound) -> Self {
		match value {
			syn::TypeParamBound::Trait(value) => value.into(),
			_ => unimplemented!(),
		}
	}
}

impl From<syn::TraitBound> for TyRef {
	fn from(value: syn::TraitBound) -> Self {
		value.path.into()
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Wrapping {
	Path(Path),
	Ref,
	RefMut,
}
