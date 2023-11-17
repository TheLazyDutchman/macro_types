#![allow(unused)]

use macro_types_helpers::From;
use proc_macro2::{Ident, Span, TokenStream};
use syn::{
	token::{
		As, At, Brace, Colon, Else, Eq, For, If, Not, Question, RArrow, Semi, Star, Underscore,
	},
	BinOp, Lit, QSelf, RangeLimits, UnOp,
};

use crate::name::{Name, Path};

pub struct Optional<T>(Option<T>);

/// This is necessary right now because our From macro does not map over an option.
impl<T, U> From<Option<T>> for Optional<U>
where
	U: From<T>,
{
	fn from(value: Option<T>) -> Self {
		Self(value.map(|x| x.into()))
	}
}

/// This is necessary right now because our From macro does not map over a vector.
pub struct List<T>(Vec<T>);

impl<I, U> FromIterator<I> for List<U>
where
	U: From<I>,
{
	fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
		Self(
			iter.into_iter()
				.map(|x| x.into())
				.collect(),
		)
	}
}

pub struct Boxed<T>(Box<T>);

impl<I, U> From<Box<I>> for Boxed<U>
where
	U: From<I>,
{
	fn from(value: Box<I>) -> Self {
		Self(Box::new((*value).into()))
	}
}

#[derive(From)]
#[from = "syn::DeriveInput.."]
pub struct DeriveInput {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	generics: Generics,
	data: Data,
}

#[derive(From)]
#[from = "syn::Data"]
pub enum Data {
	#[from(unwrap = "syn::DataStruct..")]
	Struct { fields: Fields },
	#[from(unwrap = "syn::DataEnum..")]
	Enum {
		#[from = "into_iter,collect"]
		variants: List<Variant>,
	},
	#[from(unwrap = "syn::DataUnion..")]
	Union { fields: FieldsNamed },
}

#[derive(From)]
#[from = "syn::Generics.."]
pub struct Generics {
	#[from = "into_iter,collect"]
	params: List<GenericParam>,
	where_clause: Optional<WhereClause>,
}

#[derive(From)]
#[from = "syn::GenericParam"]
pub enum GenericParam {
	#[from(unwrap = "syn::LifetimeParam..")]
	Lifetime {
		#[from = "into_iter,collect"]
		attrs: List<Attribute>,
		lifetime: Lifetime,
		#[from = "into_iter,collect"]
		bounds: List<Lifetime>,
	},

	// TODO: Add more attributes
	#[from(unwrap = "syn::TypeParam..")]
	Type {
		#[from = "into_iter,collect"]
		attrs: List<Attribute>,
		ident: Name,
		#[from = "into_iter,collect"]
		bounds: List<TypeParamBound>,
	},

	#[from(unwrap = "syn::ConstParam..")]
	Const {
		#[from = "into_iter,collect"]
		attrs: List<Attribute>,
		ident: Name,
		ty: Type,
	},
}

#[derive(From)]
#[from = "syn::Lifetime.."]
pub struct Lifetime {
	ident: Name,
}

#[derive(From)]
#[from = "syn::WhereClause.."]
pub struct WhereClause {
	#[from = "into_iter,collect"]
	predicates: List<WherePredicate>,
}

#[derive(From)]
#[from = "syn::Fields"]
pub enum Fields {
	Named(FieldsNamed),
	Unnamed(FieldsUnnamed),
	Unit,
}

#[derive(From)]
#[from = "syn::FieldsNamed.."]
pub struct FieldsNamed {
	#[from = "into_iter,collect"]
	named: List<Field>,
}

#[derive(From)]
#[from = "syn::FieldsUnnamed.."]
pub struct FieldsUnnamed {
	#[from = "into_iter,collect"]
	unnamed: List<Field>,
}

#[derive(From)]
#[from = "syn::Field.."]
pub struct Field {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Optional<Name>,
	ty: Type,
}

// TODO: Add discriminant
#[derive(From)]
#[from = "syn::Variant.."]
pub struct Variant {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	fields: Fields,
}

#[derive(From)]
#[from = "syn::Attribute.."]
pub struct Attribute {
	style: AttrStyle,
	meta: Meta,
}

#[derive(From)]
#[from = "syn::AttrStyle"]
pub enum AttrStyle {
	Outer,
	#[from(unwrap = "syn::token::Not")]
	Inner {
		spans: Spans,
	},
}

// TODO: This exist because we do not support array types yet in our macros
#[derive(derive_more::From)]
pub struct Spans([Span; 1]);

#[derive(From)]
#[from = "syn::Meta"]
pub enum Meta {
	Path(Path),
	#[from(unwrap = "syn::MetaList..")]
	List {
		path: Path,
		tokens: TokenStream,
	},
	#[from(unwrap = "syn::MetaNameValue..")]
	NameValue {
		path: Path,
		value: Expr,
	},
}

pub enum Expr {
	Array(ExprArray),
	Assign(ExprAssign),
	Async(ExprAsync),
	Await(ExprAwait),
	Binary(ExprBinary),
	Block(ExprBlock),
	Break(ExprBreak),
	Call(ExprCall),
	Cast(ExprCast),
	Closure(ExprClosure),
	Const(ExprConst),
	Continue(ExprContinue),
	Field(ExprField),
	ForLoop(ExprForLoop),
	Group(ExprGroup),
	If(ExprIf),
	Index(ExprIndex),
	Infer(ExprInfer),
	Let(ExprLet),
	Lit(ExprLit),
	Loop(ExprLoop),
	Macro(ExprMacro),
	Match(ExprMatch),
	MethodCall(ExprMethodCall),
	Paren(ExprParen),
	Path(ExprPath),
	Range(ExprRange),
	Reference(ExprReference),
	Repeat(ExprRepeat),
	Return(ExprReturn),
	Struct(ExprStruct),
	Try(ExprTry),
	TryBlock(ExprTryBlock),
	Tuple(ExprTuple),
	Verbatim(TokenStream),
	While(ExprWhile),
	Unsafe(ExprUnsafe),
	Unary(ExprUnary),
	Yield(ExprYield),
}

#[derive(From)]
#[from = "syn::ExprArray.."]
pub struct ExprArray {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	#[from = "into_iter,collect"]
	elems: List<Expr>,
}

#[derive(From)]
#[from = "syn::ExprAssign.."]
pub struct ExprAssign {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	left: Boxed<Expr>,
	right: Boxed<Expr>,
}

#[derive(From)]
#[from = "syn::ExprAsync.."]
pub struct ExprAsync {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	block: Block,
}

#[derive(From)]
#[from = "syn::ExprAwait.."]
pub struct ExprAwait {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	base: Boxed<Expr>,
}

#[derive(From)]
#[from = "syn::ExprBinary"]
pub struct ExprBinary {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	left: Boxed<Expr>,
	op: BinOp,
	right: Boxed<Expr>,
}

#[derive(From)]
#[from = "syn::ExprBlock.."]
pub struct ExprBlock {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	block: Block,
}

#[derive(From)]
#[from = "syn::ExprBreak.."]
pub struct ExprBreak {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	expr: Optional<Boxed<Expr>>,
}

#[derive(From)]
#[from = "syn::ExprCall.."]
pub struct ExprCall {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	func: Boxed<Expr>,
	#[from = "into_iter,collect"]
	args: List<Expr>,
}

#[derive(From)]
#[from = "syn::ExprCast.."]
pub struct ExprCast {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	expr: Boxed<Expr>,
	ty: Boxed<Type>,
}

#[derive(From)]
#[from = "syn::ExprClosure.."]
pub struct ExprClosure {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	lifetimes: Optional<BoundLifetimes>,
	#[from = "into_iter,collect"]
	inputs: List<Pat>,
	output: ReturnType,
	body: Boxed<Expr>,
}

#[derive(From)]
#[from = "syn::ExprConst.."]
pub struct ExprConst {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	block: Block,
}

#[derive(From)]
#[from = "syn::ExprContinue.."]
pub struct ExprContinue {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
}

#[derive(From)]
#[from = "syn::ExprField.."]
pub struct ExprField {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	base: Boxed<Expr>,
	member: Member,
}

#[derive(From)]
#[from = "syn::ExprForLoop.."]
pub struct ExprForLoop {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	pat: Boxed<Pat>,
	expr: Boxed<Expr>,
	body: Block,
}

#[derive(From)]
#[from = "syn::ExprGroup.."]
pub struct ExprGroup {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	expr: Boxed<Expr>,
}

#[derive(From)]
#[from = "syn::ExprIf.."]
pub struct ExprIf {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	cond: Boxed<Expr>,
	then_branch: Block,
	else_branch: Optional<ElsePart>,
}

pub struct ElsePart(Boxed<Expr>);

impl From<(Else, Box<syn::Expr>)> for ElsePart {
	fn from(value: (Else, Box<syn::Expr>)) -> Self {
		Self(value.1.into())
	}
}

#[derive(From)]
#[from = "syn::ExprIndex.."]
pub struct ExprIndex {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	expr: Boxed<Expr>,
	index: Boxed<Expr>,
}

#[derive(From)]
#[from = "syn::ExprInfer.."]
pub struct ExprInfer {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
}

#[derive(From)]
#[from = "syn::ExprLet.."]
pub struct ExprLet {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	pat: Boxed<Pat>,
	expr: Boxed<Expr>,
}

#[derive(From)]
#[from = "syn::ExprLit"]
pub struct ExprLit {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	lit: Lit,
}

#[derive(From)]
#[from = "syn::ExprLoop.."]
pub struct ExprLoop {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	body: Block,
}

#[derive(From)]
#[from = "syn::ExprMacro"]
pub struct ExprMacro {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	mac: Macro,
}

#[derive(From)]
#[from = "syn::Macro.."]
pub struct Macro {
	path: Path,
	tokens: TokenStream,
}

#[derive(From)]
#[from = "syn::ExprMatch.."]
pub struct ExprMatch {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	expr: Boxed<Expr>,
	#[from = "into_iter,collect"]
	arms: List<Arm>,
}

#[derive(From)]
#[from = "syn::Arm.."]
pub struct Arm {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	pat: Pat,
	guard: Optional<Guard>,
	body: Boxed<Expr>,
}

pub struct Guard(Boxed<Expr>);

impl From<(If, Box<syn::Expr>)> for Guard {
	fn from(value: (If, Box<syn::Expr>)) -> Self {
		Self(value.1.into())
	}
}

#[derive(From)]
#[from = "syn::ExprMethodCall.."]
pub struct ExprMethodCall {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	receiver: Boxed<Expr>,
	method: Name,
	turbofish: Optional<AngleBracketedGenericArguments>,
	#[from = "into_iter,collect"]
	args: List<Expr>,
}

#[derive(From)]
#[from = "syn::AngleBracketedGenericArguments.."]
pub struct AngleBracketedGenericArguments {
	#[from = "into_iter,collect"]
	args: List<GenericArgument>,
}

pub enum GenericArgument {
	Lifetime(Lifetime),
	Type(Type),
	Const(Expr),
	AssocType(AssocType),
	AssocConst(AssocConst),
	Constraint(Constraint),
}

impl From<syn::GenericArgument> for GenericArgument {
	fn from(value: syn::GenericArgument) -> Self {
		match value {
			syn::GenericArgument::Lifetime(value) => GenericArgument::Lifetime(value.into()),
			syn::GenericArgument::Type(value) => GenericArgument::Type(value.into()),
			syn::GenericArgument::Const(value) => GenericArgument::Const(value.into()),
			syn::GenericArgument::AssocType(value) => GenericArgument::AssocType(value.into()),
			syn::GenericArgument::AssocConst(value) => GenericArgument::AssocConst(value.into()),
			syn::GenericArgument::Constraint(value) => GenericArgument::Constraint(value.into()),
			_ => unimplemented!(),
		}
	}
}

#[derive(From)]
#[from = "syn::AssocType.."]
pub struct AssocType {
	ident: Name,
	generics: Optional<AngleBracketedGenericArguments>,
	ty: Type,
}

#[derive(From)]
#[from = "syn::AssocConst.."]
pub struct AssocConst {
	ident: Name,
	generics: Optional<AngleBracketedGenericArguments>,
	value: Expr,
}

#[derive(From)]
#[from = "syn::Constraint.."]
pub struct Constraint {
	ident: Name,
	generics: Optional<AngleBracketedGenericArguments>,
	#[from = "into_iter,collect"]
	bounds: List<TypeParamBound>,
}

#[derive(From)]
#[from = "syn::ExprParen.."]
pub struct ExprParen {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	expr: Boxed<Expr>,
}

#[derive(From)]
#[from = "syn::ExprPath"]
pub struct ExprPath {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	qself: Optional<QSelf>,
	path: Path,
}

#[derive(From)]
#[from = "syn::ExprRange"]
pub struct ExprRange {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	start: Optional<Boxed<Expr>>,
	limits: RangeLimits,
	end: Optional<Boxed<Expr>>,
}

#[derive(From)]
#[from = "syn::ExprReference.."]
pub struct ExprReference {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	expr: Boxed<Expr>,
}

#[derive(From)]
#[from = "syn::ExprRepeat.."]
pub struct ExprRepeat {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	expr: Boxed<Expr>,
	len: Boxed<Expr>,
}

#[derive(From)]
#[from = "syn::ExprReturn.."]
pub struct ExprReturn {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	expr: Optional<Boxed<Expr>>,
}

#[derive(From)]
#[from = "syn::ExprStruct.."]
pub struct ExprStruct {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	path: Path,
	#[from = "into_iter,collect"]
	fields: List<FieldValue>,
	rest: Optional<Boxed<Expr>>,
}

#[derive(From)]
#[from = "syn::FieldValue.."]
pub struct FieldValue {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	member: Member,
	expr: Expr,
}

#[derive(From)]
#[from = "syn::Member"]
pub enum Member {
	Named(Name),
	Unnamed(Index),
}

#[derive(From)]
#[from = "syn::ExprTry.."]
pub struct ExprTry {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	expr: Boxed<Expr>,
}

#[derive(From)]
#[from = "syn::ExprTryBlock.."]
pub struct ExprTryBlock {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	block: Block,
}

#[derive(From)]
#[from = "syn::ExprTuple.."]
pub struct ExprTuple {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	#[from = "into_iter,collect"]
	elems: List<Expr>,
}

#[derive(From)]
#[from = "syn::ExprWhile.."]
pub struct ExprWhile {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	cond: Boxed<Expr>,
	body: Block,
}

#[derive(From)]
#[from = "syn::ExprUnsafe.."]
pub struct ExprUnsafe {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	block: Block,
}

#[derive(From)]
#[from = "syn::ExprUnary"]
pub struct ExprUnary {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	op: UnOp,
	expr: Boxed<Expr>,
}

#[derive(From)]
#[from = "syn::ExprYield.."]
pub struct ExprYield {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	expr: Optional<Boxed<Expr>>,
}

impl From<syn::Expr> for Expr {
	fn from(value: syn::Expr) -> Self {
		match value {
			syn::Expr::Array(value) => Expr::Array(value.into()),
			syn::Expr::Assign(value) => Expr::Assign(value.into()),
			syn::Expr::Async(value) => Expr::Async(value.into()),
			syn::Expr::Await(value) => Expr::Await(value.into()),
			syn::Expr::Binary(value) => Expr::Binary(value.into()),
			syn::Expr::Block(value) => Expr::Block(value.into()),
			syn::Expr::Break(value) => Expr::Break(value.into()),
			syn::Expr::Call(value) => Expr::Call(value.into()),
			syn::Expr::Cast(value) => Expr::Cast(value.into()),
			syn::Expr::Closure(value) => Expr::Closure(value.into()),
			syn::Expr::Const(value) => Expr::Const(value.into()),
			syn::Expr::Continue(value) => Expr::Continue(value.into()),
			syn::Expr::Field(value) => Expr::Field(value.into()),
			syn::Expr::ForLoop(value) => Expr::ForLoop(value.into()),
			syn::Expr::Group(value) => Expr::Group(value.into()),
			syn::Expr::If(value) => Expr::If(value.into()),
			syn::Expr::Index(value) => Expr::Index(value.into()),
			syn::Expr::Infer(value) => Expr::Infer(value.into()),
			syn::Expr::Let(value) => Expr::Let(value.into()),
			syn::Expr::Lit(value) => Expr::Lit(value.into()),
			syn::Expr::Loop(value) => Expr::Loop(value.into()),
			syn::Expr::Macro(value) => Expr::Macro(value.into()),
			syn::Expr::Match(value) => Expr::Match(value.into()),
			syn::Expr::MethodCall(value) => Expr::MethodCall(value.into()),
			syn::Expr::Paren(value) => Expr::Paren(value.into()),
			syn::Expr::Path(value) => Expr::Path(value.into()),
			syn::Expr::Range(value) => Expr::Range(value.into()),
			syn::Expr::Reference(value) => Expr::Reference(value.into()),
			syn::Expr::Repeat(value) => Expr::Repeat(value.into()),
			syn::Expr::Return(value) => Expr::Return(value.into()),
			syn::Expr::Struct(value) => Expr::Struct(value.into()),
			syn::Expr::Try(value) => Expr::Try(value.into()),
			syn::Expr::TryBlock(value) => Expr::TryBlock(value.into()),
			syn::Expr::Tuple(value) => Expr::Tuple(value.into()),
			syn::Expr::Unary(value) => Expr::Unary(value.into()),
			syn::Expr::Unsafe(value) => Expr::Unsafe(value.into()),
			syn::Expr::Verbatim(value) => Expr::Verbatim(value.into()),
			syn::Expr::While(value) => Expr::While(value.into()),
			syn::Expr::Yield(value) => Expr::Yield(value.into()),
			_ => unimplemented!(),
		}
	}
}

#[derive(From)]
#[from = "syn::Block.."]
pub struct Block {
	#[from = "into_iter,collect"]
	stmts: List<Stmt>,
}

#[derive(From)]
#[from = "syn::Index.."]
pub struct Index {
	index: u32,
}

pub enum Pat {
	Const(PatConst),
	Ident(PatIdent),
	Lit(PatLit),
	Macro(PatMacro),
	Or(PatOr),
	Paren(PatParen),
	Path(PatPath),
	Range(PatRange),
	Reference(PatReference),
	Rest(PatRest),
	Slice(PatSlice),
	Struct(PatStruct),
	Tuple(PatTuple),
	TupleStruct(PatTupleStruct),
	Type(PatType),
	Verbatim(TokenStream),
	Wild(PatWild),
}
#[derive(From)]
#[from = "syn::PatConst.."]
pub struct PatConst {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	block: Block,
}

// TODO: Add byref and mutability support
#[derive(From)]
#[from = "syn::PatIdent.."]
pub struct PatIdent {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	subpat: Optional<SubPat>,
}

pub struct SubPat(Boxed<Pat>);

impl From<(At, Box<syn::Pat>)> for SubPat {
	fn from(value: (At, Box<syn::Pat>)) -> Self {
		Self(value.1.into())
	}
}

#[derive(From)]
#[from = "syn::PatLit"]
pub struct PatLit {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	lit: Lit,
}

#[derive(From)]
#[from = "syn::PatMacro"]
pub struct PatMacro {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	mac: Macro,
}

#[derive(From)]
#[from = "syn::PatOr.."]
pub struct PatOr {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	#[from = "into_iter,collect"]
	cases: List<Pat>,
}

#[derive(From)]
#[from = "syn::PatParen.."]
pub struct PatParen {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	pat: Boxed<Pat>,
}

#[derive(From)]
#[from = "syn::PatPath"]
pub struct PatPath {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	qself: Optional<QSelf>,
	path: Path,
}

#[derive(From)]
#[from = "syn::PatRange"]
pub struct PatRange {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	start: Optional<Boxed<Expr>>,
	limits: RangeLimits,
	end: Optional<Boxed<Expr>>,
}

#[derive(From)]
#[from = "syn::PatReference.."]
pub struct PatReference {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	pat: Boxed<Pat>,
}

#[derive(From)]
#[from = "syn::PatRest.."]
pub struct PatRest {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
}

#[derive(From)]
#[from = "syn::PatSlice.."]
pub struct PatSlice {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	#[from = "into_iter,collect"]
	elems: List<Pat>,
}

#[derive(From)]
#[from = "syn::PatStruct.."]
pub struct PatStruct {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	qself: Optional<QSelf>,
	path: Path,
	#[from = "into_iter,collect"]
	fields: List<FieldPat>,
	rest: Optional<PatRest>,
}

#[derive(From)]
#[from = "syn::FieldPat.."]
pub struct FieldPat {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	member: Member,
	pat: Boxed<Pat>,
}

#[derive(From)]
#[from = "syn::PatTuple.."]
pub struct PatTuple {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	#[from = "into_iter,collect"]
	elems: List<Pat>,
}

#[derive(From)]
#[from = "syn::PatTupleStruct.."]
pub struct PatTupleStruct {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	qself: Optional<QSelf>,
	path: Path,
	#[from = "into_iter,collect"]
	elems: List<Pat>,
}

#[derive(From)]
#[from = "syn::PatType.."]
pub struct PatType {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	pat: Boxed<Pat>,
	ty: Boxed<Type>,
}

#[derive(From)]
#[from = "syn::PatWild.."]
pub struct PatWild {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
}

impl From<syn::Pat> for Pat {
	fn from(value: syn::Pat) -> Self {
		match value {
			syn::Pat::Const(value) => Pat::Const(value.into()),
			syn::Pat::Ident(value) => Pat::Ident(value.into()),
			syn::Pat::Lit(value) => Pat::Lit(value.into()),
			syn::Pat::Macro(value) => Pat::Macro(value.into()),
			syn::Pat::Or(value) => Pat::Or(value.into()),
			syn::Pat::Paren(value) => Pat::Paren(value.into()),
			syn::Pat::Path(value) => Pat::Path(value.into()),
			syn::Pat::Range(value) => Pat::Range(value.into()),
			syn::Pat::Reference(value) => Pat::Reference(value.into()),
			syn::Pat::Rest(value) => Pat::Rest(value.into()),
			syn::Pat::Slice(value) => Pat::Slice(value.into()),
			syn::Pat::Struct(value) => Pat::Struct(value.into()),
			syn::Pat::Tuple(value) => Pat::Tuple(value.into()),
			syn::Pat::TupleStruct(value) => Pat::TupleStruct(value.into()),
			syn::Pat::Type(value) => Pat::Type(value.into()),
			syn::Pat::Verbatim(value) => Pat::Verbatim(value.into()),
			syn::Pat::Wild(value) => Pat::Wild(value.into()),
			_ => unimplemented!(),
		}
	}
}

#[derive(From)]
#[from = "syn::ReturnType"]
pub enum ReturnType {
	Default,
	Type(RArrow, Boxed<Type>),
}

#[derive(From)]
#[from = "syn::BoundLifetimes.."]
pub struct BoundLifetimes {
	#[from = "into_iter,collect"]
	lifetimes: List<GenericParam>,
}

#[derive(From)]
#[from = "syn::Stmt"]
pub enum Stmt {
	#[from(unwrap = "syn::Local..")]
	Local {
		#[from = "into_iter,collect"]
		attrs: List<Attribute>,
		pat: Pat,
		init: Optional<LocalInit>,
	},
	Item(Item),
	Expr(Expr, Option<Semi>),
	#[from(unwrap = "syn::StmtMacro..")]
	Macro {
		#[from = "into_iter,collect"]
		attrs: List<Attribute>,
		mac: Macro,
	},
}

#[derive(From)]
#[from = "syn::LocalInit.."]
pub struct LocalInit {
	expr: Boxed<Expr>,
	diverge: Optional<Diverge>,
}

pub struct Diverge(Boxed<Expr>);

impl From<(Else, Box<syn::Expr>)> for Diverge {
	fn from(value: (Else, Box<syn::Expr>)) -> Self {
		Self(value.1.into())
	}
}

pub enum Item {
	Const(ItemConst),
	Enum(ItemEnum),
	ExternCrate(ItemExternCrate),
	Fn(ItemFn),
	ForeignMod(ItemForeignMod),
	Impl(ItemImpl),
	Macro(ItemMacro),
	Mod(ItemMod),
	Static(ItemStatic),
	Struct(ItemStruct),
	Trait(ItemTrait),
	TraitAlias(ItemTraitAlias),
	Type(ItemType),
	Union(ItemUnion),
	Use(ItemUse),
	Verbatim(TokenStream),
}

#[derive(From)]
#[from = "syn::ItemConst.."]
pub struct ItemConst {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	generics: Generics,
	ty: Boxed<Type>,
	expr: Boxed<Expr>,
}

#[derive(From)]
#[from = "syn::ItemEnum.."]
pub struct ItemEnum {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	generics: Generics,
	#[from = "into_iter,collect"]
	variants: List<Variant>,
}

#[derive(From)]
#[from = "syn::ItemExternCrate.."]
pub struct ItemExternCrate {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	rename: Optional<AsName>,
}

pub struct AsName(Name);

impl From<(As, Ident)> for AsName {
	fn from(value: (As, Ident)) -> Self {
		Self(value.1.into())
	}
}

#[derive(From)]
#[from = "syn::ItemFn.."]
pub struct ItemFn {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	sig: Signature,
	block: Boxed<Block>,
}

#[derive(From)]
#[from = "syn::ItemForeignMod.."]
pub struct ItemForeignMod {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	#[from = "into_iter,collect"]
	items: List<ForeignItem>,
}

pub enum ForeignItem {
	Fn(ForeignItemFn),
	Static(ForeignItemStatic),
	Type(ForeignItemType),
	Macro(ForeignItemMacro),
	Verbatim(TokenStream),
}

#[derive(From)]
#[from = "syn::ForeignItemFn.."]
pub struct ForeignItemFn {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	sig: Signature,
}

#[derive(From)]
#[from = "syn::ForeignItemStatic.."]
pub struct ForeignItemStatic {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	ty: Boxed<Type>,
}

#[derive(From)]
#[from = "syn::ForeignItemType.."]
pub struct ForeignItemType {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	generics: Generics,
}

#[derive(From)]
#[from = "syn::ForeignItemMacro.."]
pub struct ForeignItemMacro {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	mac: Macro,
}

impl From<syn::ForeignItem> for ForeignItem {
	fn from(value: syn::ForeignItem) -> Self {
		match value {
			syn::ForeignItem::Fn(value) => ForeignItem::Fn(value.into()),
			syn::ForeignItem::Static(value) => ForeignItem::Static(value.into()),
			syn::ForeignItem::Type(value) => ForeignItem::Type(value.into()),
			syn::ForeignItem::Macro(value) => ForeignItem::Macro(value.into()),
			syn::ForeignItem::Verbatim(value) => ForeignItem::Verbatim(value.into()),
			_ => unimplemented!(),
		}
	}
}

#[derive(From)]
#[from = "syn::ItemImpl.."]
pub struct ItemImpl {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	generics: Generics,
	trait_: Optional<ForTrait>,
	self_ty: Boxed<Type>,
	#[from = "into_iter,collect"]
	items: List<ImplItem>,
}

pub enum ImplItem {
	Const(ImplItemConst),
	Fn(ImplItemFn),
	Type(ImplItemType),
	Macro(ImplItemMacro),
	Verbatim(TokenStream),
}
#[derive(From)]
#[from = "syn::ImplItemConst.."]
pub struct ImplItemConst {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	generics: Generics,
	ty: Type,
	expr: Expr,
}

#[derive(From)]
#[from = "syn::ImplItemFn.."]
pub struct ImplItemFn {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	sig: Signature,
	block: Block,
}

#[derive(From)]
#[from = "syn::ImplItemType.."]
pub struct ImplItemType {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	generics: Generics,
	ty: Type,
}

#[derive(From)]
#[from = "syn::ImplItemMacro.."]
pub struct ImplItemMacro {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	mac: Macro,
}

impl From<syn::ImplItem> for ImplItem {
	fn from(value: syn::ImplItem) -> Self {
		match value {
			syn::ImplItem::Const(value) => ImplItem::Const(value.into()),
			syn::ImplItem::Fn(value) => ImplItem::Fn(value.into()),
			syn::ImplItem::Type(value) => ImplItem::Type(value.into()),
			syn::ImplItem::Macro(value) => ImplItem::Macro(value.into()),
			syn::ImplItem::Verbatim(value) => ImplItem::Verbatim(value.into()),
			_ => unimplemented!(),
		}
	}
}

pub struct ForTrait(Option<Not>, Path);

impl From<(Option<Not>, syn::Path, For)> for ForTrait {
	fn from(value: (Option<Not>, syn::Path, For)) -> Self {
		Self(value.0, value.1.into())
	}
}

#[derive(From)]
#[from = "syn::ItemMacro.."]
pub struct ItemMacro {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Optional<Name>,
	mac: Macro,
}

#[derive(From)]
#[from = "syn::ItemMod.."]
pub struct ItemMod {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	content: Optional<ModItems>,
}

pub struct ModItems(List<Item>);

impl From<(Brace, Vec<syn::Item>)> for ModItems {
	fn from(value: (Brace, Vec<syn::Item>)) -> Self {
		Self(value.1.into_iter().collect())
	}
}

#[derive(From)]
#[from = "syn::ItemStatic.."]
pub struct ItemStatic {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	ty: Boxed<Type>,
	expr: Boxed<Expr>,
}

#[derive(From)]
#[from = "syn::ItemStruct.."]
pub struct ItemStruct {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	generics: Generics,
	fields: Fields,
}

#[derive(From)]
#[from = "syn::ItemTrait.."]
pub struct ItemTrait {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	generics: Generics,
	#[from = "into_iter,collect"]
	supertraits: List<TypeParamBound>,
	#[from = "into_iter,collect"]
	items: List<TraitItem>,
}

pub enum TraitItem {
	Const(TraitItemConst),
	Fn(TraitItemFn),
	Type(TraitItemType),
	Macro(TraitItemMacro),
	Verbatim(TokenStream),
}

#[derive(From)]
#[from = "syn::TraitItemConst.."]
pub struct TraitItemConst {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	generics: Generics,
	ty: Type,
	default: Optional<DefaultExpr>,
}

pub struct DefaultExpr(Expr);

impl From<(Eq, syn::Expr)> for DefaultExpr {
	fn from(value: (Eq, syn::Expr)) -> Self {
		Self(value.1.into())
	}
}

#[derive(From)]
#[from = "syn::TraitItemFn.."]
pub struct TraitItemFn {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	sig: Signature,
	default: Optional<Block>,
}

#[derive(From)]
#[from = "syn::TraitItemType.."]
pub struct TraitItemType {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	generics: Generics,
	#[from = "into_iter,collect"]
	bounds: List<TypeParamBound>,
	default: Optional<DefaultType>,
}

pub struct DefaultType(Type);

impl From<(Eq, syn::Type)> for DefaultType {
	fn from(value: (Eq, syn::Type)) -> Self {
		Self(value.1.into())
	}
}

#[derive(From)]
#[from = "syn::TraitItemMacro.."]
pub struct TraitItemMacro {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	mac: Macro,
}

impl From<syn::TraitItem> for TraitItem {
	fn from(value: syn::TraitItem) -> Self {
		match value {
			syn::TraitItem::Const(value) => TraitItem::Const(value.into()),
			syn::TraitItem::Fn(value) => TraitItem::Fn(value.into()),
			syn::TraitItem::Type(value) => TraitItem::Type(value.into()),
			syn::TraitItem::Macro(value) => TraitItem::Macro(value.into()),
			syn::TraitItem::Verbatim(value) => TraitItem::Verbatim(value.into()),
			_ => unimplemented!(),
		}
	}
}

#[derive(From)]
#[from = "syn::ItemTraitAlias.."]
pub struct ItemTraitAlias {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	generics: Generics,
	#[from = "into_iter,collect"]
	bounds: List<TypeParamBound>,
}

#[derive(From)]
#[from = "syn::ItemType.."]
pub struct ItemType {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	generics: Generics,
	ty: Boxed<Type>,
}

#[derive(From)]
#[from = "syn::ItemUnion.."]
pub struct ItemUnion {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ident: Name,
	generics: Generics,
	fields: FieldsNamed,
}

#[derive(From)]
#[from = "syn::ItemUse.."]
pub struct ItemUse {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	tree: UseTree,
}

#[derive(From)]
#[from = "syn::UseTree"]
pub enum UseTree {
	#[from(unwrap = "syn::UsePath..")]
	Path { ident: Name, tree: Boxed<UseTree> },
	#[from(unwrap = "syn::UseName")]
	Name { ident: Name },
	#[from(unwrap = "syn::UseRename..")]
	Rename { ident: Name, rename: Name },
	#[from(unwrap = "syn::UseGlob..")]
	Glob { star_token: Star },
	#[from(unwrap = "syn::UseGroup..")]
	Group {
		#[from = "into_iter,collect"]
		items: List<UseTree>,
	},
}

impl From<syn::Item> for Item {
	fn from(value: syn::Item) -> Self {
		match value {
			syn::Item::Const(value) => Item::Const(value.into()),
			syn::Item::Enum(value) => Item::Enum(value.into()),
			syn::Item::ExternCrate(value) => Item::ExternCrate(value.into()),
			syn::Item::Fn(value) => Item::Fn(value.into()),
			syn::Item::ForeignMod(value) => Item::ForeignMod(value.into()),
			syn::Item::Impl(value) => Item::Impl(value.into()),
			syn::Item::Macro(value) => Item::Macro(value.into()),
			syn::Item::Mod(value) => Item::Mod(value.into()),
			syn::Item::Static(value) => Item::Static(value.into()),
			syn::Item::Struct(value) => Item::Struct(value.into()),
			syn::Item::Trait(value) => Item::Trait(value.into()),
			syn::Item::TraitAlias(value) => Item::TraitAlias(value.into()),
			syn::Item::Type(value) => Item::Type(value.into()),
			syn::Item::Union(value) => Item::Union(value.into()),
			syn::Item::Use(value) => Item::Use(value.into()),
			syn::Item::Verbatim(value) => Item::Verbatim(value.into()),
			_ => unimplemented!(),
		}
	}
}

pub enum TypeParamBound {
	Trait(TraitBound),
	Lifetime(Lifetime),
	Verbatim(TokenStream),
}

#[derive(From)]
#[from = "syn::TraitBound.."]
pub struct TraitBound {
	modifier: TraitBoundModifier,
	lifetimes: Optional<BoundLifetimes>,
	path: Path,
}

#[derive(From)]
#[from = "syn::TraitBoundModifier"]
pub enum TraitBoundModifier {
	None,
	Maybe(Question),
}

impl From<syn::TypeParamBound> for TypeParamBound {
	fn from(value: syn::TypeParamBound) -> Self {
		match value {
			syn::TypeParamBound::Trait(value) => TypeParamBound::Trait(value.into()),
			syn::TypeParamBound::Lifetime(value) => TypeParamBound::Lifetime(value.into()),
			syn::TypeParamBound::Verbatim(value) => TypeParamBound::Verbatim(value.into()),
			_ => unimplemented!(),
		}
	}
}

pub enum WherePredicate {
	Lifetime(PredicateLifetime),
	Type(PredicateType),
}

#[derive(From)]
#[from = "syn::PredicateLifetime.."]
pub struct PredicateLifetime {
	lifetime: Lifetime,
	#[from = "into_iter,collect"]
	bounds: List<Lifetime>,
}

#[derive(From)]
#[from = "syn::PredicateType.."]
pub struct PredicateType {
	lifetimes: Optional<BoundLifetimes>,

	bounded_ty: Type,

	#[from = "into_iter,collect"]
	bounds: List<TypeParamBound>,
}

impl From<syn::WherePredicate> for WherePredicate {
	fn from(value: syn::WherePredicate) -> Self {
		match value {
			syn::WherePredicate::Lifetime(value) => WherePredicate::Lifetime(value.into()),
			syn::WherePredicate::Type(value) => WherePredicate::Type(value.into()),
			_ => unimplemented!(),
		}
	}
}

pub enum Type {
	Array(TypeArray),
	BareFn(TypeBareFn),
	Group(TypeGroup),
	ImplTrait(TypeImplTrait),
	Infer(TypeInfer),
	Macro(TypeMacro),
	Never(TypeNever),
	Paren(TypeParen),
	Path(TypePath),
	Ptr(TypePtr),
	Reference(TypeReference),
	Slice(TypeSlice),
	TraitObject(TypeTraitObject),
	Tuple(TypeTuple),
	Verbatim(TokenStream),
}

#[derive(From)]
#[from = "syn::TypeArray.."]
pub struct TypeArray {
	elem: Boxed<Type>,
	len: Expr,
}

#[derive(From)]
#[from = "syn::TypeBareFn.."]
pub struct TypeBareFn {
	#[from = "into_iter,collect"]
	inputs: List<BareFnArg>,
	output: ReturnType,
}

#[derive(From)]
#[from = "syn::BareFnArg"]
pub struct BareFnArg {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	name: Optional<BareFnArgName>,
	ty: Type,
}

pub struct BareFnArgName(Name);

impl From<(Ident, Colon)> for BareFnArgName {
	fn from(value: (Ident, Colon)) -> Self {
		Self(value.0.into())
	}
}

#[derive(From)]
#[from = "syn::TypeGroup.."]
pub struct TypeGroup {
	elem: Boxed<Type>,
}

#[derive(From)]
#[from = "syn::TypeImplTrait.."]
pub struct TypeImplTrait {
	#[from = "into_iter,collect"]
	bounds: List<TypeParamBound>,
}

#[derive(From)]
#[from = "syn::TypeInfer"]
pub struct TypeInfer {
	underscore_token: Underscore,
}

#[derive(From)]
#[from = "syn::TypeMacro"]
pub struct TypeMacro {
	mac: Macro,
}

#[derive(From)]
#[from = "syn::TypeNever"]
pub struct TypeNever {
	// TODO: Find a way to use the from derive macro to produce unit structs
	bang_token: Not,
}

#[derive(From)]
#[from = "syn::TypeParen.."]
pub struct TypeParen {
	elem: Boxed<Type>,
}

#[derive(From)]
#[from = "syn::TypePath"]
pub struct TypePath {
	qself: Optional<QSelf>,
	path: Path,
}

#[derive(From)]
#[from = "syn::TypePtr.."]
pub struct TypePtr {
	elem: Boxed<Type>,
}

#[derive(From)]
#[from = "syn::TypeReference.."]
pub struct TypeReference {
	elem: Boxed<Type>,
}

#[derive(From)]
#[from = "syn::TypeSlice.."]
pub struct TypeSlice {
	elem: Boxed<Type>,
}

#[derive(From)]
#[from = "syn::TypeTraitObject.."]
pub struct TypeTraitObject {
	#[from = "into_iter,collect"]
	bounds: List<TypeParamBound>,
}

#[derive(From)]
#[from = "syn::TypeTuple.."]
pub struct TypeTuple {
	#[from = "into_iter,collect"]
	elems: List<Type>,
}

impl From<syn::Type> for Type {
	fn from(value: syn::Type) -> Self {
		match value {
			syn::Type::Array(value) => Type::Array(value.into()),
			syn::Type::BareFn(value) => Type::BareFn(value.into()),
			syn::Type::Group(value) => Type::Group(value.into()),
			syn::Type::ImplTrait(value) => Type::ImplTrait(value.into()),
			syn::Type::Infer(value) => Type::Infer(value.into()),
			syn::Type::Macro(value) => Type::Macro(value.into()),
			syn::Type::Never(value) => Type::Never(value.into()),
			syn::Type::Paren(value) => Type::Paren(value.into()),
			syn::Type::Path(value) => Type::Path(value.into()),
			syn::Type::Ptr(value) => Type::Ptr(value.into()),
			syn::Type::Reference(value) => Type::Reference(value.into()),
			syn::Type::Slice(value) => Type::Slice(value.into()),
			syn::Type::TraitObject(value) => Type::TraitObject(value.into()),
			syn::Type::Tuple(value) => Type::Tuple(value.into()),
			syn::Type::Verbatim(value) => Type::Verbatim(value.into()),
			_ => unimplemented!(),
		}
	}
}

#[derive(From)]
#[from = "syn::Signature.."]
pub struct Signature {
	ident: Name,
	generics: Generics,
	#[from = "into_iter,collect"]
	inputs: List<FnArg>,
	variadic: Optional<Variadic>,
	output: ReturnType,
}

#[derive(From)]
#[from = "syn::Variadic.."]
pub struct Variadic {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	pat: Optional<VariadicPat>,
}

pub struct VariadicPat(Boxed<Pat>);

impl From<(Box<syn::Pat>, Colon)> for VariadicPat {
	fn from(value: (Box<syn::Pat>, Colon)) -> Self {
		Self(value.0.into())
	}
}

#[derive(From)]
#[from = "syn::FnArg"]
pub enum FnArg {
	Receiver(Receiver),
	Typed(PatType),
}

// TODO: Add support for mutability
#[derive(From)]
#[from = "syn::Receiver.."]
pub struct Receiver {
	#[from = "into_iter,collect"]
	attrs: List<Attribute>,
	ty: Boxed<Type>,
}
