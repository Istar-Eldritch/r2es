mod expression;
mod if_stat;

use nom::bytes::complete::{tag, take_while};
use nom::character::{is_alphabetic, is_alphanumeric, is_space, is_newline};
use nom::{IResult};
use nom_locate::{LocatedSpan};

use self::expression::parse_expression;
use self::if_stat::parse_if;

use super::ast::{
    ESClass, ESClassAttributes, ESDeclare, ESFn, ESFnArg, ESBlock, ESStatement,
    ESType, Span, ESAsign,
};

#[derive(Debug, PartialEq)]
pub enum ESError<'a> {
    InvalidInput(Span<'a>, &'a str),
    Unexpected,
    EmptyInput(Span<'a>),
}

fn take_spaces(s: Span) -> Result<(Span, Span), ESError> {
    take_while::<_,_,()>(|c: char| c.is_ascii() && is_space(c as u8) || is_newline(c as u8))(s).map_err(|_| ESError::Unexpected)
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

fn parse_declaration(s: Span) -> Result<(Span, ESDeclare), ESError> {
    let (s, _) = take_spaces(s)?;
    let mut constant = false;
    let mut s = s;
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
        let (ss, v) = parse_expression(ss)?;
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
    let (s, value) = parse_expression(s)?;
    Ok((s, ESAsign {ident: String::from(ident.trim()), value}))
}

fn parse_statement(s: Span) -> Result<(Span, ESStatement), ESError> {
    let (s, _) = take_spaces(s)?;
    let mut s = s;
    let result =
    if let Ok((ss, value)) = parse_if(s) {
        s = ss;
        Ok(ESStatement::If(Box::new(value)))
    } else if let Ok((ss, value)) = parse_declaration(s) {
        let (ss, _) = take_spaces(ss)?;
        let (ss, _) = tag::<_,_,()>(";")(ss).map_err(|_| ESError::InvalidInput(ss, ";"))?;
        s = ss;
        Ok(ESStatement::Declare(value))
    } else if let Ok((ss, value)) = parse_asignment(s) {
        let (ss, _) = take_spaces(ss)?;
        let (ss, _) = tag::<_,_,()>(";")(ss).map_err(|_| ESError::InvalidInput(ss, ";"))?;
        s = ss;
        Ok(ESStatement::Assign(value))
    } else if let Ok((ss, value)) = parse_expression(s) {
        let (ss, _) = take_spaces(ss)?;
        let (ss, _) = tag::<_,_,()>(";")(ss).map_err(|_| ESError::InvalidInput(ss, ";"))?;
        s = ss;
        Ok(ESStatement::Expression(value))
    } else {
        Err(ESError::InvalidInput(s, "Statemnt"))?
    };
    result.map(|r| (s, r))
}

fn parse_block(s: Span) -> Result<(Span, ESBlock), ESError> {
    let (s, _) = take_spaces(s)?;
    let (s, _) = tag::<_,_,()>("{")(s).map_err(|_| ESError::InvalidInput(s, "{"))?;
    let (s, _) = take_spaces(s)?;
    let mut s = s;
    let mut statements: Vec<ESStatement>  = Vec::new();
    while let Ok((ss, stm)) = parse_statement(s) {
        statements.push(stm);
        (s, _) = take_spaces(ss)?;
        
    }
    let (s, _) = take_spaces(s)?;
    let (s, _) = tag::<_,_,()>("}")(s).map_err(|_| ESError::InvalidInput(s, "}"))?;
    Ok((s, ESBlock {statements}))
}

fn parse_function(s: Span) -> Result<(Span, ESFn), ESError> {
    let (s, out_typ) = parse_type(s)?;
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
    let (s, body) = parse_block(s)?;

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

    use nom_locate::LocatedSpan;

    use crate::es::{ast::{ESDeclare, ESLiteral, ESType, ESAsign, ESExpression, ESGroup, ESIfBranch, ESIf}};

    use super::*;

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
                    value: Some(crate::es::ast::ESExpression::Literal(ESLiteral::Int(1)))
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
                    value: ESExpression::Literal(ESLiteral::Int(1))
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
                    value: ESExpression::Literal(ESLiteral::Int(1))
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
                    value: ESExpression::Literal(ESLiteral::Float(2.5))
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
                    value: ESExpression::Literal(ESLiteral::Bool(false))
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
                    value: ESExpression::Literal(ESLiteral::String("hello".into()))
                }
            ))
        );
    }

    #[test]
    fn parse_block_0() {
        let s = LocatedSpan::from("{}");
        let dec = parse_block(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(2, 1, "", ()) },
                ESBlock {
                    statements: vec![]
                }
            ))
        );
    }

    #[test]
    fn parse_block_1() {
        let s = LocatedSpan::from("{int a = 1;}");
        let dec = parse_block(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(12, 1, "", ()) },
                ESBlock {
                    statements: vec![
                        ESStatement::Declare(ESDeclare { ident: "a".into(), constant: false, typ: ESType::Int, value: Some(ESExpression::Literal(ESLiteral::Int(1))) })
                    ]
                }
            ))
        );
    }


    #[test]
    fn parse_block_2() {
        let s = LocatedSpan::from("{
            int a = 1;
            a = 2;
        }");
        let dec = parse_block(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(53, 4, "", ()) },
                ESBlock {
                    statements: vec![
                        ESStatement::Declare(ESDeclare { ident: "a".into(), constant: false, typ: ESType::Int, value: Some(ESExpression::Literal(ESLiteral::Int(1))) }),
                        ESStatement::Assign(ESAsign { ident: "a".into(), value: ESExpression::Literal(ESLiteral::Int(2)) })
                    
                    ]
                }
            ))
        );
    }

    #[test]
    fn parse_block_3() {
        let s = LocatedSpan::from("{
            if(true) {}
        }");
        let dec = parse_block(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(35, 3, "", ()) },
                ESBlock {
                    statements: vec![
                        ESStatement::If(Box::new(ESIf {branches: vec![
                            ESIfBranch {
                                body: ESBlock { statements: vec![] },
                                conditional: ESExpression::Literal(ESLiteral::Bool(true))
                            }
                        ]}))
                    
                    ]
                }
            ))
        );
    }

}
