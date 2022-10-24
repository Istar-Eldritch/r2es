use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

pub enum Item<'s> {
    Class(ESClass<'s>),
    Fn(),
}

pub struct ESAsign<'s> {
    pub location: Span<'s>,
    pub ident: &'s str,
    pub typ: ESType,
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

pub enum ESStatement<'s> {
    Declare(ESDeclare<'s>),
    Asign(ESAsign<'s>),
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

pub struct ESFn<'s> {
    pub location: Span<'s>,
    pub ident: &'s str,
    pub out_typ: ESType,
    pub args: Vec<ESFnArg<'s>>,
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
