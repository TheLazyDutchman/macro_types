use quote::{quote, ToTokens};

use crate::name::{Name, Path, TyRef};

pub enum Error {
	WrongVariant {
		expected: &'static str,
		found: &'static str,
	},
}

macro_rules! conversions {
	(pub enum $name:ident {
        $($([$is_value:ident])? $variant:ident{$($var_name:ident: $var_ty:ty),*} => $tokens:tt),*
        $(,)?
    }) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
		pub enum $name {
		    $($variant($variant)),*
		}

		paste::paste! {
		    impl [<$name Value>] for $name {}
		}

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

            conversions!(@new_func $variant { $($var_name: $var_ty),* });

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
                type Error = Error;

                fn try_into(self) -> std::result::Result<$variant, Self::Error> {
                    paste::paste! {
                        self.[<into_ $variant:snake>]().map_err(|x| Error::WrongVariant{ expected: stringify!($variant), found: x.variant_name() })
                    }
                }
            }

            paste::paste! {
                impl [<$name Value>] for $variant {}
            }

            impl quote::ToTokens for $variant {
                fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
                    $(
                        let $var_name = &self.$var_name;
                    )*

                    tokens.extend(quote::quote! $tokens)
                }
            }
        )*

	    impl $name {
	        paste::paste! {
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
	    }

	    paste::paste! {
	        pub trait [<$name Value>] {
	            $(
	                conversions!(@trait_func $($is_value)? $name::$variant{ $($var_name: $var_ty),* });
	            )*
	        }
	    }

	    impl quote::ToTokens for $name {
	        fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
	            match self {
	                $(
	                    $name::$variant(value) => value.to_tokens(tokens)
	                ),*
	            }
	        }
	    }
	};
	(@trait_func value $name:ident::$variant:ident{ $field:ident: $first_ty:ty $(, $var_name:ident: $var_ty:ty)* }) => {
	    paste::paste! {
	        fn [<$variant:snake>](&self, $($var_name: impl Into<$var_ty>),*) -> $variant where Self: Clone + Into<Expr> {
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
}

conversions! {
	pub enum Expr {
		Variable { path: Path } => { #path },
		[value] Field { value: Box<Expr>, field: Name } => { #value.#field },
		[value] Unwrap { value: Box<Expr> } => { #value? },
		[value] Assign { variable: Box<Expr>, value: Box<Expr> } => { #variable = #value },
		[value] Call { func: Box<Expr>, args: Vec<Expr> } => { #func(#(#args),*) },
		Block { exprs: Vec<Expr> } => {{ #(#exprs);* }},
		Constructor { owner: TyRef, fields: Vec<ConstructorField> } => { #owner { #(#fields),* }}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstructorField {
	name: Name,
	expr: Expr,
}

impl ToTokens for ConstructorField {
	fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
		let name = &self.name;
		let expr = &self.expr;

		tokens.extend(quote! {
			#name: #expr
		})
	}
}

impl Constructor {
	pub fn add_field(&mut self, name: impl Into<Name>, expr: impl Into<Expr>) {
		self.fields
			.push(ConstructorField {
				name: name.into(),
				expr: expr.into(),
			})
	}
}

impl<T> ExprValue for T where Path: From<T> {}
impl<T> From<T> for Expr
where
	Path: From<T>,
{
	fn from(value: T) -> Self {
		Expr::Variable(Variable::new(value))
	}
}
