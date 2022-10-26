use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

pub enum Item<'s> {
    Class(ESClass<'s>),
    Fn(),
}

#[derive(Debug, PartialEq)]
pub struct ESAsign<'s> {
    pub location: Span<'s>,
    pub ident: &'s str,
    pub value: ESLiteral<'s>,
}

#[derive(Debug, PartialEq)]
pub struct ESDeclare<'s> {
    pub location: Span<'s>,
    pub ident: &'s str,
    pub constant: bool,
    pub typ: ESType,
    pub value: Option<ESLiteral<'s>>,
}

pub enum ESOperandValue {
    Add,
    Substract,
    Multiply,
    Divide,
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    Ne,
    And,
    Or
}

pub struct ESOperand<'a> {
    pub location: Span<'a>,
    pub value: ESOperandValue
}

pub struct ESBinOp<'a> {
    pub location: Span<'a>,
    pub left: ESExpression<'a>,
    pub right: ESExpression<'a>,
    pub op: ESOperand<'a>
}

pub enum ESPrefixOperandValue {
    Negate
}

pub struct ESPrefixOperand<'a> {
    pub location: Span<'a>,
    pub value: ESPrefixOperandValue
}

pub struct ESPrefixOp<'a> {
    pub location: Span<'a>,
    pub value: ESExpression<'a>,
    pub op: ESPrefixOperand<'a>
}

pub enum ESExpressionValue<'a> {
    Assign(ESAsign<'a>),
    Declare(ESDeclare<'a>),
    BinOp(Box<ESBinOp<'a>>),
    PrefixOp(Box<ESPrefixOp<'a>>)
}

pub struct ESExpression<'a> {
    pub location: Span<'a>,
    pub value: ESExpressionValue<'a>
}

pub enum ESStatementValue<'a> {
    Expression(ESExpression<'a>)
}

pub struct ESStatement<'a> {
    pub location: Span<'a>,
    pub value: ESStatementValue<'a>
}

pub type ESClassAttributes<'s> = Vec<ESDeclare<'s>>;

pub struct ESClass<'s> {
    pub location: Span<'s>,
    pub ident: &'s str,
    pub attributes: ESClassAttributes<'s>,
}

pub struct ESFnArg<'s> {
    pub location: Span<'s>,
    pub ident: &'s str,
    pub typ: ESType,
    pub by_ref: bool,
    pub default: Option<ESLiteral<'s>>,
}

pub struct ESFnBody<'s> {
    pub location: Span<'s>,
    pub statements: Vec<ESStatement<'s>>,
}

pub struct ESFn<'s> {
    pub location: Span<'s>,
    pub ident: &'s str,
    pub out_typ: ESType,
    pub args: Vec<ESFnArg<'s>>,
    pub body: ESFnBody<'s>,
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
pub struct ESString<'s> {
    pub location: Span<'s>,
    pub value: &'s str,
}

#[derive(Debug, PartialEq)]
pub enum ESLiteral<'s> {
    Bool(bool),
    Int(u64),
    Float(f64),
    String(ESString<'s>),
}
