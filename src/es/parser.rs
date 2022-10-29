
use std::cell::RefCell;

use nom::bytes::complete::{tag, take_while, take_until, take_until1, take};
use nom::character::{is_alphabetic, is_alphanumeric, is_digit, is_space};
use nom::IResult;
use nom_locate::{position, LocatedSpan};

use super::ast::{
    ESClass, ESClassAttributes, ESDeclare, ESFn, ESFnArg, ESFnBody, ESLiteral, ESStatement,
    ESType, Span, ESExpression, ESAsign, ESGroup, ESUnary, ESUnaryOp, ESFactor, ESFactorOp, ESTerm, ESTermOp, ESCmp, ESCmpOp, ESEqOp, ESEq,
};

#[derive(Debug, PartialEq)]
pub enum ESError<'a> {
    InvalidInput(Span<'a>, &'a str),
    Unexpected,
    EmptyInput(Span<'a>),
}

fn take_spaces(s: Span) -> Result<(Span, Span), ESError> {
    take_while::<_,_,()>(|c: char| c.is_ascii() && is_space(c as u8))(s).map_err(|_| ESError::Unexpected)
}

fn parse_type(s: Span) -> Result<(Span, ESType), ESError> {
    let (s, r) = take_while::<_,_,()>(|ch: char| is_alphanumeric(ch as u8))(s).map_err(|_|ESError::Unexpected)?;
    match r.trim() {
        "bool" => Ok((s, ESType::Bool)),
        "class" => Ok((s, ESType::Class)),
        "float" => Ok((s, ESType::Float)),
        "int" => Ok((s, ESType::Int)),
        "string" => Ok((s, ESType::String)),
        "typename" => Ok((s, ESType::Typename)),
        "vector" => Ok((s, ESType::Vector)),
        "void" => Ok((s, ESType::Void)),
        _ => Err(ESError::InvalidInput(s, "Type")),
    }
}

fn parse_ident(s: Span) -> Result<(Span, Span), ESError> {
    if s.len() == 0 {
        return Err(ESError::EmptyInput(s));
    }
    take_while::<_,_,()>(|c: char| c.is_ascii() && is_alphabetic(c as u8))(s).map_err(|_| ESError::Unexpected)
}

fn parse_bool(s: Span) -> IResult<Span, bool> {
    if let Ok((s, _)) = tag::<&str, Span, ()>("true")(s) {
        Ok((s, true))
    } else if let Ok((s, _)) = tag::<&str, Span, ()>("false")(s) {
        Ok((s, false))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(
            s,
            nom::error::ErrorKind::IsNot,
        )))
    }
}

fn parse_int(s: Span) -> IResult<Span, u64> {
    let (s, r) = take_while(|ch: char| ch.is_ascii() && is_digit(ch as u8) || ch == '.')(s)?;
    r.trim()
        .parse::<u64>()
        .map(|v| ((s, v)))
        .map_err(|_| nom::Err::Error(nom::error::Error::new(s, nom::error::ErrorKind::IsNot)))
}

fn parse_float(s: Span) -> IResult<Span, f64> {
    let (s, r) = take_while(|ch: char| ch.is_ascii() && (is_digit(ch as u8) || ch == '.'))(s)?;
    r.trim()
        .parse::<f64>()
        .map(|v| ((s, v)))
        .map_err(|_| nom::Err::Error(nom::error::Error::new(s, nom::error::ErrorKind::IsNot)))
}

fn parse_str(s: Span) -> IResult<Span, String> {
    let (s, quoted) = take_while(|ch: char| ch.is_ascii() && (is_alphanumeric(ch as u8) || ch == '"'))(s)?;
    let (value, _) = tag("\"")(quoted)?;
    let value = take_while(|ch: char| ch.is_ascii() && is_alphanumeric(ch as u8))(value);
    let (pending, r) = (value).map(
        |(s, inner)| {
            (
                s,
                String::from(inner.trim()),
            )
        },
    )?;
    tag("\"")(pending)?;
    Ok((s, r))
}

fn parse_literal(s: Span) -> Result<(Span, ESLiteral), ESError> {
    if let Ok((s, r)) = parse_bool(s) {
        Ok((s, ESLiteral::Bool(r)))
    } else if let Ok((s, r)) = parse_int(s) {
        Ok((s, ESLiteral::Int(r)))
    } else if let Ok((s, r)) = parse_float(s) {
        Ok((s, ESLiteral::Float(r)))
    } else if let Ok((s, r)) = parse_str(s) {
        Ok((s, ESLiteral::String(r)))
    } else {
        Err(ESError::InvalidInput(s, "Literal"))
    }
}

fn parse_declaration(s: Span) -> Result<(Span, ESDeclare), ESError> {
    let (s, _) = take_spaces(s)?;
    let mut constant = false;
    let mut s = s;
    let location = s;
    if let Ok((ss, _)) = tag::<&str, LocatedSpan<&str>, ()>("const")(s) {
        constant = true;
        let (ss, _) = take_spaces(ss)?;
        s = ss;
    }

    let (s, typ) = parse_type(s)?;
    let (s, _) = take_spaces(s)?;

    let (s, ident) = parse_ident(s)?;

    let mut value = None;

    let (s, _) = take_spaces(s)?;
    let mut s = s;
    if let Ok((ss, _)) = tag::<&str, Span, ()>("=")(s) {
        let (ss, _) = take_spaces(ss)?;
        let (ss, v) = parse_literal(ss)?;
        s = ss;
        value = Some(v);
    }

    Ok((
        s,
        ESDeclare {
            ident: String::from(ident.trim()),
            constant,
            typ,
            value,
        },
    ))
}

fn parse_function_arg(s: Span) -> Result<(Span, ESFnArg), ESError> {
    let (location, _) = take_spaces(s)?;
    let (location, typ) = parse_type(s)?;
    let (s, _) = take_spaces(s)?;
    let (s, ident) = parse_ident(s)?;
    Ok((
        s,
        ESFnArg {
            ident: String::from(ident.trim()),
            typ,
            by_ref: false,
            default: None,
        },
    ))
}

fn parse_asignment(s: Span) -> Result<(Span, ESAsign), ESError> {
    let (s, _) = take_spaces(s)?;
    let (s, ident) = parse_ident(s)?;

    let (s, _) = take_spaces(s)?;
    let (s, _) = tag::<_,_,()>("=")(s).map_err(|_| ESError::InvalidInput(s, "Equals"))?;
    let (s, _) = take_spaces(s)?;
    let (s, value) = parse_literal(s)?;
    Ok((s, ESAsign {ident: String::from(ident.trim()), value}))
}


fn parse_primary(s: Span) -> Result<(Span, ESExpression), ESError> {
    if let Ok((s, parsed)) = parse_literal(s) {
        Ok((s, ESExpression::Literal(parsed)))
    } else if let Ok((s, parsed)) = parse_group(s) {
        Ok((s, ESExpression::Group(Box::new(parsed))))
    } else {
        Err(ESError::InvalidInput(s, "Not valid primary"))
    }
}

fn parse_unary(s: Span) -> Result<(Span, ESExpression), ESError> {
    let (s, op) = take_while::<_,_,()>(|ch| ch == '-' || ch == '!')(s).map_err(|_| ESError::InvalidInput(s, "UnaryOperator"))?;
    let op = match op.trim() {
        "-" => ESUnaryOp::Negate,
        "!" => ESUnaryOp::LogicalNegate,
        _ => return Err(ESError::InvalidInput(s, "UnaryOperator"))
    };

    if let Ok((s, expr)) = parse_unary(s) {
        Ok((s, ESExpression::Unary(Box::new(ESUnary { op, expr }))))
    } else if let Ok((s, expr)) = parse_primary(s) {
        Ok((s, ESExpression::Unary(Box::new(ESUnary { op, expr }))))
    } else {
        Err(ESError::InvalidInput(s, "UnaryExpression"))
    }
}

fn parse_factor(s: Span) -> Result<(Span, ESFactor), ESError> {
    let is_factor_op= |ch| ch == '*' || ch == '/';
    let (s, left) = take_while::<_,_,()>(|c| !is_factor_op(c))(s).map_err(|_| ESError::InvalidInput(s, "LeftFactorOperand"))?;
    let (s, op) = take_while::<_,_,()>(is_factor_op)(s).map_err(|_| ESError::InvalidInput(s, "FactorOperator"))?;
    let op = match op.trim() {
        "*" => ESFactorOp::Multiply,
        "/" => ESFactorOp::Divide,
        _ => return Err(ESError::InvalidInput(s, "FactorOperator"))
    };
    let (_, left) = parse_expression(left)?;
    let (s, right) = parse_expression(s)?;
    Ok((s, ESFactor { op, left, right }))
}

fn parse_term(s: Span) -> Result<(Span, ESTerm), ESError> {
    let ini = s;
    let is_term_op= |ch| ch == '+' || ch == '-';
    let (s, left) = take_while::<_,_,()>(|c| !is_term_op(c))(s).map_err(|_| ESError::InvalidInput(s, "LeftTermOperand"))?;
    let (s, op) = take_while::<_,_,()>(is_term_op)(s).map_err(|_| ESError::InvalidInput(s, "TermOperator"))?;
    let op = match op.trim() {
        "+" => ESTermOp::Add,
        "-" => ESTermOp::Substract,
        _ => return Err(ESError::InvalidInput(op, "TermOperator"))
    };
    let (_, left) = parse_expression(left)?;
    let (s, right) = parse_expression(s)?;
    println!("in: {ini} op: {op:?} rem: {s}");
    Ok((s, ESTerm { op, left, right }))
}

fn parse_eq(s: Span) -> Result<(Span, ESEq), ESError> {
    let is_eq_op= |ch| ch == '=' || ch == '1';
    let (s, left) = take_while::<_,_,()>(|c| !is_eq_op(c))(s).map_err(|_| ESError::InvalidInput(s, "LeftEqOperand"))?;
    let (s, op) = take_while::<_,_,()>(is_eq_op)(s).map_err(|_| ESError::InvalidInput(s, "EqOperator"))?;
    let op = match op.trim() {
        "==" => ESEqOp::Eq,
        "!=" => ESEqOp::Ne,
        _ => return Err(ESError::InvalidInput(s, "EqOperator"))
    };
    let (_, left) = parse_expression(left)?;
    let (s, right) = parse_expression(s)?;
    Ok((s, ESEq { op, left, right }))
}

fn parse_cmp(s: Span) -> Result<(Span, ESCmp), ESError> {
    let is_cmp_op= |ch| ch == '>' || ch == '<' || ch == '=';
    let (s, left) = take_while::<_,_,()>(|c| !is_cmp_op(c))(s).map_err(|_| ESError::InvalidInput(s, "LeftCmpOperand"))?;
    let (s, op) = take_while::<_,_,()>(is_cmp_op)(s).map_err(|_| ESError::InvalidInput(s, "CmpOperator"))?;
    let op = match op.trim() {
        ">" => ESCmpOp::Gt,
        "<" => ESCmpOp::Lt,
        ">=" => ESCmpOp::Gte,
        "<=" => ESCmpOp::Lte,
        _ => return Err(ESError::InvalidInput(s, "CmpOperator"))
    };
    let (_, left) = parse_expression(left)?;
    let (s, right) = parse_expression(s)?;
    Ok((s, ESCmp { op, left, right }))
}

fn parse_group(s: Span) -> Result<(Span, ESGroup), ESError> {
    let ini = s;
    let (s, _) = tag::<_,_,()>("(")(s).map_err(|_| ESError::InvalidInput(s, "("))?;
    let paren_stack = RefCell::new(vec!['(']);
    let unbalanced: RefCell<bool> = RefCell::new(false);
    let (s, inner_span) = take_while::<_,_,()>(|c| {
        match c {
            '(' => paren_stack.borrow_mut().push('('),
            ')' =>  {
                if let Some('(') = paren_stack.borrow_mut().pop() {
                } else {
                    *unbalanced.borrow_mut() = true;
                    return false; 
                }
            },
            _ => ()
        };
        !paren_stack.borrow().is_empty()
    })(s).map_err(|_| ESError::Unexpected)?;
    if *unbalanced.borrow() {
        return Err( ESError::InvalidInput(s, "Unbalanced parens"))
    }
    let (_, inner) = parse_expression(inner_span)?;
    let (s, _) = tag::<_,_,()>(")")(s).map_err(|_| ESError::InvalidInput(s, ")"))?;
    println!("toparse: {ini}, inner: {inner_span} rem: {s}");
    Ok((s, ESGroup { inner }))
}

fn parse_expression(s: Span) -> Result<(Span, ESExpression), ESError> {
    let (s, _) = take_spaces(s)?;
    if s.is_empty() {
        return Err(ESError::EmptyInput(s))
    }
    // TODO: Parse comparators
    // TODO: Parse logical operators
    // TODO: Parse binary operators
    // TODO: Test unary factor and term
    if let Ok((s, value)) = parse_unary(s) {
        Ok((s, ESExpression::Unary(Box::new(value))))
    } else if let Ok((s, value)) = parse_factor(s) {
        Ok((s, ESExpression::Factor(Box::new(value))))
    } else if let Ok((s, value)) = parse_term(s) {
        Ok((s, ESExpression::Term(Box::new(value))))
    } else if let Ok((s, value)) = parse_cmp(s) {
        Ok((s, ESExpression::Cmp(Box::new(value))))
    } else if let Ok((s, value)) = parse_group(s) {
        Ok((s, ESExpression::Group(Box::new(value))))
    } else if let Ok((s, value)) = parse_literal(s) {
        Ok((s, ESExpression::Literal(value)))
    } else {
        Err(ESError::InvalidInput(s, "Expression"))
    }
}

fn parse_statement(s: Span) -> Result<(Span, ESStatement), ESError> {
    let mut s = s;
    let result =
    if let Ok((ss, value)) = parse_declaration(s) {
        s = ss;
        Ok(ESStatement::Declare(value))
    } else if let Ok((ss, value)) = parse_asignment(s) {
        s = ss;
        Ok(ESStatement::Assign(value))
    } else if let Ok((ss, value)) = parse_expression(s) {
        s = ss;
        Ok(ESStatement::Expression(value))
    } else {
        unimplemented!()
    };
    let (s, _) = take_spaces(s)?;
    let (s, _) = tag::<_,_,()>(";")(s).map_err(|_| ESError::InvalidInput(s, ";"))?;
    result.map(|r| (s, r))
}

fn parse_function_body(mut s: Span) -> Result<(Span, ESFnBody), ESError> {
    let (s, _) = take_spaces(s)?;
    let (s, _) = tag::<_,_,()>("{")(s).map_err(|_| ESError::InvalidInput(s, "{"))?;
    let location = s;
    let (s, _) = take_spaces(s)?;
    let mut s = s;
    let mut statements: Vec<ESStatement>  = Vec::new();
    while let Ok((ss, stm)) = parse_statement(s) {
        statements.push(stm);
        (s, _) = take_spaces(ss)?;
        
    }
    let (s, _) = tag::<_,_,()>("}")(s).map_err(|_| ESError::InvalidInput(s, "}"))?;
    Ok((s, ESFnBody {statements}))
}

fn parse_function(s: Span) -> Result<(Span, ESFn), ESError> {
    let (s, out_typ) = parse_type(s)?;
    let location = s;
    let (s, _) = take_spaces(s)?;
    let (s, ident) = parse_ident(s)?;
    let (s, _) = tag::<_,_,()>("(")(s).map_err(|_| ESError::InvalidInput(s, "("))?;
    let mut s = s;
    let mut args: Vec<ESFnArg> = Vec::new();
    let mut closing_tag: IResult<Span, Span>;
    let mut first = true;
    loop {
        let (ss, _) = take_spaces(s)?;
        s = ss;
        closing_tag = tag(")")(s);
        if let Ok((ss, _)) = closing_tag {
            s = ss;
            break;
        } else {
            if !first {
                let (ss, _) = tag::<_,_,()>(",")(s).map_err(|_| ESError::InvalidInput(s, ","))?;
                let (ss, _) = take_spaces(ss)?;
                s = ss;
                first = false;
            }
            let (ss, arg) = parse_function_arg(s)?;
            args.push(arg);
            s = ss;
        }
    }
    let (s, body) = parse_function_body(s)?;

    Ok((
        s,
        ESFn {
            ident: String::from(ident.trim()),
            out_typ,
            args,
            body,
        },
    ))
}

fn parse_class_attributes(s: Span) -> Result<(Span, ESClassAttributes), ESError> {
    let mut s: Span = s;
    let mut attributes: Vec<ESDeclare> = Vec::new();
    while let Ok((ss, attribute)) = parse_declaration(s) {
        s = ss;
        attributes.push(attribute);
    }
    Ok((s, attributes))
}

fn parse_class(s: Span) -> Result<(Span, ESClass), ESError> {
    let (s, _) = tag::<_,_,()>("class")(s).map_err(|_| ESError::InvalidInput(s, "class"))?;
    let (s, _) = take_spaces(s)?;
    let (s, ident) = parse_ident(s)?;
    let (s, _) = take_spaces(s)?;
    let (s, _) = tag::<_,_,()>("{")(s).map_err(|_| ESError::InvalidInput(s, "{"))?;
    let (s, attributes) = parse_class_attributes(s)?;
    // TODO:  parse body
    let (s, _) = tag::<_,_,()>("}")(s).map_err(|_| ESError::InvalidInput(s, "}"))?;

    Ok((
        s,
        ESClass {
            ident: String::from(ident.trim()),
            attributes,
        },
    ))
}

#[cfg(test)]
mod tests {

    use nom::IResult;
    use nom_locate::LocatedSpan;

    use crate::es::{ast::{ESDeclare, ESLiteral, ESType, ESAsign, ESGroup, ESExpression, ESTerm, ESTermOp, ESFactor, ESFactorOp}, parser::{parse_asignment, parse_group, parse_term, parse_expression}};

    use super::{parse_declaration, parse_ident};

    #[test]
    fn parses_identities() {
        let mut s = LocatedSpan::from("");
        let dec = parse_ident(s);
        assert!(dec.is_err());
        s = LocatedSpan::from("abc");
        let dec = parse_ident(s);
        assert_eq!(
            dec,
            Ok((unsafe { LocatedSpan::new_from_raw_offset(3, 1, "", ()) }, s))
        );
        s = LocatedSpan::from("abc;");
        let dec = parse_ident(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(3, 1, ";", ()) },
                unsafe { LocatedSpan::new_from_raw_offset(0, 1, "abc", ()) }
            ))
        );
    }

    #[test]
    fn parses_declarations() {
        let mut s = LocatedSpan::from("");
        let dec = parse_declaration(s);
        assert!(dec.is_err());
        s = LocatedSpan::from("int a");
        let dec = parse_declaration(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(5, 1, "", ()) },
                ESDeclare {
                    ident: "a".into(),
                    constant: false,
                    typ: ESType::Int,
                    value: None
                }
            ))
        );

        s = LocatedSpan::from("const int a");
        let dec = parse_declaration(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(11, 1, "", ()) },
                ESDeclare {
                    ident: "a".into(),
                    constant: true,
                    typ: ESType::Int,
                    value: None
                }
            ))
        );

        s = LocatedSpan::from("const int a = 1");
        let dec = parse_declaration(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(15, 1, "", ()) },
                ESDeclare {
                    ident: "a".into(),
                    constant: true,
                    typ: ESType::Int,
                    value: Some(ESLiteral::Int(1))
                }
            ))
        );
    }

    #[test]
    fn parses_assignments() {
        let mut s = LocatedSpan::from("");
        let dec = parse_asignment(s);
        assert!(dec.is_err());
        s = LocatedSpan::from("a = 1");
        let dec = parse_asignment(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(5, 1, "", ()) },
                ESAsign {
                    ident: "a".into(),
                    value: ESLiteral::Int(1)
                }
            ))
        );

        s = LocatedSpan::from("a=1");
        let dec = parse_asignment(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(3, 1, "", ()) },
                ESAsign {
                    ident: "a".into(),
                    value: ESLiteral::Int(1)
                }
            ))
        );

        s = LocatedSpan::from("a=2.5");
        let dec = parse_asignment(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(5, 1, "", ()) },
                ESAsign {
                    ident: "a".into(),
                    value: ESLiteral::Float(2.5)
                }
            ))
        );

        s = LocatedSpan::from("a=false");
        let dec = parse_asignment(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(7, 1, "", ()) },
                ESAsign {
                    ident: "a".into(),
                    value: ESLiteral::Bool(false)
                }
            ))
        );

        s = LocatedSpan::from("a=\"hello\"");
        let dec = parse_asignment(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(9, 1, "", ()) },
                ESAsign {
                    ident: "a".into(),
                    value: ESLiteral::String("hello".into())
                }
            ))
        );
    }

    #[test]
    fn parses_terms() {
        let mut s = LocatedSpan::from("");
        let dec = parse_term(s);
        assert!(dec.is_err());

        s = LocatedSpan::from("1 +");
        let dec = parse_term(s);
        assert!(dec.is_err());

        s = LocatedSpan::from("+ 1");
        let dec = parse_term(s);
        assert!(dec.is_err());

        s = LocatedSpan::from("1 + 2");
        let dec = parse_term(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(5, 1, "", ()) },
                ESTerm {
                    left: ESExpression::Literal(ESLiteral::Int(1)),
                    right: ESExpression::Literal(ESLiteral::Int(2)),
                    op: ESTermOp::Add
                }
            ))
        );

        s = LocatedSpan::from("1 - 2");
        let dec = parse_term(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(5, 1, "", ()) },
                ESTerm {
                    left: ESExpression::Literal(ESLiteral::Int(1)),
                    right: ESExpression::Literal(ESLiteral::Int(2)),
                    op: ESTermOp::Substract
                }
            ))
        );

    }

    #[test]
    fn parses_groups() {
        let mut s = LocatedSpan::from("");
        let dec = parse_group(s);
        assert!(dec.is_err());
        s = LocatedSpan::from("()");
        let dec = parse_group(s);
        assert!(dec.is_err());

        s = LocatedSpan::from("( 1 )");
        let dec = parse_group(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(5, 1, "", ()) },
                ESGroup {
                    inner: crate::es::ast::ESExpression::Literal(ESLiteral::Int(1))
                }
            ))
        );

        s = LocatedSpan::from("((1)+(2))");
        let dec = parse_group(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(9, 1, "", ()) },
                ESGroup {
                    inner: crate::es::ast::ESExpression::Term(Box::new(ESTerm {
                        left: crate::es::ast::ESExpression::Group(Box::new(ESGroup {
                            inner: crate::es::ast::ESExpression::Literal(ESLiteral::Int(1))
                        })),
                        right: crate::es::ast::ESExpression::Group(Box::new(ESGroup {
                            inner: crate::es::ast::ESExpression::Literal(ESLiteral::Int(2))
                        })),
                        op: crate::es::ast::ESTermOp::Add
                    }))
                    
                }
            ))
        );

        s = LocatedSpan::from("((1+2)+3)");
        let dec = parse_group(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(9, 1, "", ()) },
                ESGroup {
                    inner: crate::es::ast::ESExpression::Term(Box::new(ESTerm {
                        left: crate::es::ast::ESExpression::Group(Box::new(ESGroup {
                            inner: crate::es::ast::ESExpression::Term(Box::new(ESTerm {
                                left: ESExpression::Literal(ESLiteral::Int(1)),
                                right: ESExpression::Literal(ESLiteral::Int(2)),
                                op: ESTermOp::Add
                            }))
                        })),
                        right: crate::es::ast::ESExpression::Group(Box::new(ESGroup {
                            inner: crate::es::ast::ESExpression::Literal(ESLiteral::Int(3))
                        })),
                        op: crate::es::ast::ESTermOp::Add
                    }))
                    
                }
            ))
        );
    }

    #[test]
    fn parses_expressions() {

        let s = LocatedSpan::from("(2 + 3) * 5");
        let dec = parse_expression(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(11, 1, "", ()) },
                ESExpression::Factor(Box::new(ESFactor {
                    left: ESExpression::Group(Box::new(ESGroup { inner: ESExpression::Term(Box::new(ESTerm {
                        left: ESExpression::Literal(ESLiteral::Int(2)),
                        right: ESExpression::Literal(ESLiteral::Int(3)),
                        op: ESTermOp::Add
                    }))})),
                    right: ESExpression::Literal(ESLiteral::Int(5)),
                    op: ESFactorOp::Multiply
                }))
            ))
        );
        
    }
}
