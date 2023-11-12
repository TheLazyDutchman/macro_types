use attr::Attr;
use expr::{Block, Constructor};
use name::Name;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{Data, DeriveInput};
use tyref::TyRef;

pub mod attr;
pub mod expr;
pub mod name;
pub mod tyref;

pub enum Error {
	WrongVariant {
		expected: &'static str,
		found: &'static str,
	},
}

#[macro_export]
macro_rules! enum_definitions {
	(
		pub enum $name:ident {
        	$($([$is_value:ident])? $variant:ident{$($var_name:ident: $var_ty:ty),*} $(=> $tokens:tt)?),*
        	$(,)?
    	}
	) => {
		paste::paste! {
        	#[derive(Debug, Clone, PartialEq, Eq)]
			pub enum $name {
		    	$($variant($variant)),*
			}

		    impl [<$name Value>] for $name {}

			$(
            	#[derive(Debug, Clone, PartialEq, Eq)]
            	pub struct $variant{
                	$(pub $var_name: $var_ty),*
            	}

            	impl $variant {
                	pub fn new($($var_name: impl Into<$var_ty>),*) -> Self {
                    	Self {
                        	$(
                            	$var_name: $var_name.into()
                        	),*
                    	}
                	}
            	}

            	crate::enum_definitions!(@new_func $variant { $($var_name: $var_ty),* });

            	impl From<$variant> for $name {
                	fn from(value: $variant) -> $name {
                    	Self::$variant(value)
                	}
            	}

            	impl From<$variant> for std::boxed::Box<$name> {
                	fn from(value: $variant) -> std::boxed::Box<$name> {
                    	std::boxed::Box::new($name::$variant(value))
                	}
            	}

            	impl TryInto<$variant> for $name {
                	type Error = crate::Error;

                	fn try_into(self) -> std::result::Result<$variant, Self::Error> {
                    	paste::paste! {
                        	self.[<into_ $variant:snake>]().map_err(|x| crate::Error::WrongVariant{ expected: stringify!($variant), found: x.variant_name() })
                    	}
                	}
            	}

                impl [<$name Value>] for $variant {}

        	)*
			crate::enum_definitions!(@to_tokens $name {
				$($variant { $($var_name: $var_ty),* } $(=> $tokens)?),*
			});

	    	impl $name {
	            $(
                    pub fn [<is_ $variant:snake>](&self) -> bool {
                        matches!(self, Self::$variant(_))
                    }

                    pub fn [<as_ $variant:snake>](&self) -> std::result::Result<&$variant, &Self> {
                        match self {
                            Self::$variant(value) => Ok(value),
                            _ => Err(self)
                        }
                    }

                    pub fn [<into_ $variant:snake>](self) -> std::result::Result<$variant, Self> {
                        match self {
                            Self::$variant(value) => Ok(value),
                            _ => Err(self)
                        }
                    }
                )*

                fn variant_name(&self) -> &'static str {
                    match self {
                        $(
                            Self::$variant(_) => stringify!($variant),
                        )*
                    }
                }
	    	}

	        pub trait [<$name Value>] {
	            $(
	                crate::enum_definitions!(@trait_func $($is_value)? $name::$variant{ $($var_name: $var_ty),* });
	            )*
	        }
		}
	};
	(@trait_func value $name:ident::$variant:ident{ $field:ident: $first_ty:ty $(, $var_name:ident: $var_ty:ty)* }) => {
	    paste::paste! {
	        fn [<$variant:snake>](&self, $($var_name: impl Into<$var_ty>),*) -> $variant where Self: Clone + Into<$name> {
	            $variant {
	                $field: Box::new(self.clone().into()),
	                $(
	                    $var_name: $var_name.into(),
	                )*
	            }
	        }
	    }
	};
	(@trait_func $name:ident::$variant:ident{ $($var_name:ident: $var_ty:ty),* }) => {};
	(@new_func $name:ident { $field:ident: $ty:ty }) => {
	    impl From<$ty> for $name {
	        fn from(value: $ty) -> $name {
	            Self::new(value)
	        }
	    }

	    impl std::ops::Deref for $name {
	        type Target = $ty;

	        fn deref(&self) -> &$ty {
	            &self.$field
	        }
	    }

	    impl std::ops::DerefMut for $name {
	        fn deref_mut(&mut self) ->&mut $ty {
	            &mut self.$field
	        }
	    }
	};
	(@new_func $name:ident { $($field:ident: $ty:ty),* }) => {};
	(@to_tokens $name:ident {
        $($([$is_value:ident])? $variant:ident{$($var_name:ident: $var_ty:ty),*} => $tokens:tt),*
        $(,)?
    }) => {
	    impl quote::ToTokens for $name {
	        fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
	            match self {
	                $(
	                    $name::$variant(value) => value.to_tokens(tokens)
	                ),*
	            }
	        }
	    }

	    $(
	    	impl quote::ToTokens for $variant {
	    		fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
	    			$(let $var_name = &self.$var_name;)*
	    			tokens.extend(quote::quote!($tokens));
	    		}
	    	}
	    )*
    };
	(@to_tokens $name:ident {
        $($([$is_value:ident])? $variant:ident{$($var_name:ident: $var_ty:ty),*}),*
        $(,)?
    }) => {};
}

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
		Constructor::new(self.name.clone(), vec![])
	}

	pub fn method(
		&self,
		name: impl Into<Name>,
		kind: FunctionKind,
		args: Vec<Arg>,
		ret_ty: Option<TyRef>,
		block: impl Into<Block>,
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
	for_trait: Option<TyRef>,
	tyref: TyRef,
	functions: Vec<Function>,
}

impl ImplBlock {
	pub fn new(tyref: impl Into<TyRef>) -> Self {
		Self {
			for_trait: None,
			tyref: tyref.into(),
			functions: Vec::new(),
		}
	}

	pub fn function(
		&mut self,
		name: impl Into<Name>,
		kind: FunctionKind,
		args: Vec<Arg>,
		ret_ty: Option<impl Into<TyRef>>,
		block: impl Into<Block>,
	) {
		let mut function = Function::new(name, kind, args, ret_ty, block);
		if self.for_trait.is_some() {
			function.is_pub = false;
		}

		self.functions.push(function);
	}

	pub fn for_trait(&mut self, for_trait: impl Into<TyRef>) -> &mut Self {
		self.for_trait = Some(for_trait.into());
		self
	}
}

impl ToTokens for ImplBlock {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let tyref = &self.tyref;
		let functions = &self.functions;
		let trait_ref = self
			.for_trait
			.as_ref()
			.map(|x| quote!(#x for));

		tokens.extend(quote! {
			impl #trait_ref #tyref {
				#(#functions)*
			}
		})
	}
}

#[derive(Debug, Clone)]
pub struct Function {
	pub is_pub: bool,
	pub name: Name,
	pub kind: FunctionKind,
	pub args: Vec<Arg>,
	pub ret_ty: Option<TyRef>,
	pub block: Block,
}

impl Function {
	pub fn new(
		name: impl Into<Name>,
		kind: FunctionKind,
		args: Vec<Arg>,
		ret_ty: Option<impl Into<TyRef>>,
		block: impl Into<Block>,
	) -> Function {
		Self {
			is_pub: true,
			name: name.into(),
			kind,
			args,
			ret_ty: ret_ty.map(|x| x.into()),
			block: block.into(),
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

		let is_pub = self
			.is_pub
			.then_some(quote!(pub));

		tokens.extend(quote! {
			#is_pub fn #name(#(#args),*) #ret {
				#block

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
