use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

pub enum Item{
    Class(ESClass),
    Fn(),
}

#[derive(Debug, PartialEq)]
pub struct ESAsign {
    pub ident: String,
    pub value: ESLiteral,
}

#[derive(Debug, PartialEq)]
pub struct ESDeclare {
    pub ident: String,
    pub constant: bool,
    pub typ: ESType,
    pub value: Option<ESLiteral>,
}

#[derive(Debug, PartialEq)]
pub struct ESGroup {
    pub inner: ESExpression
}

#[derive(Debug, PartialEq)]
pub enum ESUnaryOp {
    Negate,
    LogicalNegate
}

#[derive(Debug, PartialEq)]
pub struct ESUnary {
    pub op: ESUnaryOp,
    pub expr: ESExpression
}

#[derive(Debug, PartialEq)]
pub enum ESFactorOp {
    Multiply,
    Divide
}

#[derive(Debug, PartialEq)]
pub struct ESFactor {
    pub op: ESFactorOp,
    pub left: ESExpression,
    pub right: ESExpression
}

#[derive(Debug, PartialEq)]
pub enum ESTermOp {
    Add,
    Substract
}

#[derive(Debug, PartialEq)]
pub struct ESTerm {
    pub op: ESTermOp,
    pub left: ESExpression,
    pub right: ESExpression
}

#[derive(Debug, PartialEq)]
pub enum ESEqOp {
    Eq,
    Ne
}

#[derive(Debug, PartialEq)]
pub struct ESEq {
    pub op: ESEqOp,
    pub left: ESExpression,
    pub right: ESExpression
}

#[derive(Debug, PartialEq)]
pub enum ESCmpOp {
    Lt,
    Gt,
    Lte,
    Gte
}

#[derive(Debug, PartialEq)]
pub struct ESCmp {
    pub op: ESCmpOp,
    pub left: ESExpression,
    pub right: ESExpression
}

#[derive(Debug, PartialEq)]
pub enum ESExpression {
    Unary(Box<ESUnary>),
    Factor(Box<ESFactor>),
    Term(Box<ESTerm>),
    Cmp(Box<ESCmp>),
    Literal(ESLiteral),
    Group(Box<ESGroup>)
}

pub enum ESStatement {
    Expression(ESExpression),
    Assign(ESAsign),
    Declare(ESDeclare),
}

pub type ESClassAttributes = Vec<ESDeclare>;

pub struct ESClass {
    pub ident: String,
    pub attributes: ESClassAttributes,
}

pub struct ESFnArg {
    pub ident: String,
    pub typ: ESType,
    pub by_ref: bool,
    pub default: Option<ESLiteral>,
}

pub struct ESFnBody {
    pub statements: Vec<ESStatement>,
}

pub struct ESFn {
    pub ident: String,
    pub out_typ: ESType,
    pub args: Vec<ESFnArg>,
    pub body: ESFnBody,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ESType {
    Int,
    Float,
    Bool,
    String,
    Vector,
    Void,
    Class,
    Typename,
}

#[derive(Debug, PartialEq)]
pub enum ESLiteral {
    Bool(bool),
    Int(u64),
    Float(f64),
    String(String),
}
