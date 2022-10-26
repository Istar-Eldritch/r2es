
use nom::bytes::complete::{tag, take_while};
use nom::character::{is_alphabetic, is_alphanumeric, is_digit, is_space};
use nom::IResult;
use nom_locate::{position, LocatedSpan};

use super::ast::{
    ESClass, ESClassAttributes, ESDeclare, ESFn, ESFnArg, ESFnBody, ESLiteral, ESStatement,
    ESString, ESType, Span, ESStatementValue, ESExpression, ESAsign,
};

fn take_spaces(s: Span) -> IResult<Span, Span> {
    take_while(|c: char| c.is_ascii() && is_space(c as u8))(s)
}

fn parse_type(s: Span) -> IResult<Span, ESType> {
    let (s, r) = take_while(|ch: char| is_alphanumeric(ch as u8))(s)?;
    match r.trim() {
        "bool" => Ok((s, ESType::Bool)),
        "class" => Ok((s, ESType::Class)),
        "float" => Ok((s, ESType::Float)),
        "int" => Ok((s, ESType::Int)),
        "string" => Ok((s, ESType::String)),
        "typename" => Ok((s, ESType::Typename)),
        "vector" => Ok((s, ESType::Vector)),
        "void" => Ok((s, ESType::Void)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            s,
            nom::error::ErrorKind::IsNot,
        ))),
    }
}

fn parse_ident(s: Span) -> IResult<Span, Span> {
    if s.len() == 0 {
        return Err(nom::Err::Error(nom::error::Error::new(
            s,
            nom::error::ErrorKind::Fail,
        )));
    }
    take_while(|c: char| c.is_ascii() && is_alphabetic(c as u8))(s)
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

fn parse_str(s: Span) -> IResult<Span, ESString> {
    let (s, quoted) = take_while(|ch: char| ch.is_ascii() && (is_alphanumeric(ch as u8) || ch == '"'))(s)?;
    let (value, _) = tag("\"")(quoted)?;
    let value = take_while(|ch: char| ch.is_ascii() && is_alphanumeric(ch as u8))(value);
    let (pending, r) = (value).map(
        |(s, inner)| {
            (
                s,
                ESString {
                    location: quoted,
                    value: inner.trim(),
                },
            )
        },
    )?;
    tag("\"")(pending)?;
    Ok((s, r))
}

fn parse_literal(s: Span) -> IResult<Span, ESLiteral> {
    if let Ok((s, r)) = parse_bool(s) {
        Ok((s, ESLiteral::Bool(r)))
    } else if let Ok((s, r)) = parse_int(s) {
        Ok((s, ESLiteral::Int(r)))
    } else if let Ok((s, r)) = parse_float(s) {
        Ok((s, ESLiteral::Float(r)))
    } else if let Ok((s, r)) = parse_str(s) {
        Ok((s, ESLiteral::String(r)))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(
            s,
            nom::error::ErrorKind::IsNot,
        )))
    }
}

fn parse_declaration(s: Span) -> IResult<Span, ESDeclare> {
    let (s, _) = take_while(|c: char| c.is_ascii() && is_space(c as u8))(s)?;
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
            location,
            ident: ident.trim(),
            constant,
            typ,
            value,
        },
    ))
}

fn parse_function_arg(s: Span) -> IResult<Span, ESFnArg> {
    let (location, _) = take_spaces(s)?;
    let (location, typ) = parse_type(s)?;
    let (s, _) = take_spaces(s)?;
    let (s, ident) = parse_ident(s)?;
    Ok((
        s,
        ESFnArg {
            location,
            ident: ident.trim(),
            typ,
            by_ref: false,
            default: None,
        },
    ))
}

fn parse_asignment(s: Span) -> IResult<Span, ESAsign> {
    let (s, _) = take_spaces(s)?;
    let location = s;
    let (s, ident) = parse_ident(s)?;

    let (s, _) = take_spaces(s)?;
    let (s, _) = tag("=")(s)?;
    let (s, _) = take_spaces(s)?;
    let (s, value) = parse_literal(s)?;
    Ok((s, ESAsign {location, ident: ident.trim(), value}))
}

fn parse_statement(s: Span) -> IResult<Span, ESStatement> {
    let mut s = s;
    let result =
    if let Ok((ss, value)) = parse_declaration(s) {
        s = ss;
        Ok(ESStatement { location: s, value: ESStatementValue::Expression(ESExpression { location: s, value: super::ast::ESExpressionValue::Declare(value)})})
    } else if let Ok((ss, value)) = parse_asignment(s) {
        s = ss;
        Ok(ESStatement { location: s, value: ESStatementValue::Expression(ESExpression { location: s, value: super::ast::ESExpressionValue::Assign(value)})})
    } else {
        unimplemented!()
    };
    let (s, _) = take_spaces(s)?;
    let (s, _) = tag(";")(s)?;
    result.map(|r| (s, r))
}

fn parse_function_body(mut s: Span) -> IResult<Span, ESFnBody> {
    let (s, _) = take_spaces(s)?;
    let (s, _) = tag("{")(s)?;
    let location = s;
    let (s, _) = take_spaces(s)?;
    let mut s = s;
    let mut statements: Vec<ESStatement>  = Vec::new();
    while let Ok((ss, stm)) = parse_statement(s) {
        statements.push(stm);
        (s, _) = take_spaces(ss)?;
        
    }
    let (s, _) = tag("}")(s)?;
    Ok((s, ESFnBody {location, statements}))
}

fn parse_function(s: Span) -> IResult<Span, ESFn> {
    let (s, out_typ) = parse_type(s)?;
    let location = s;
    let (s, _) = take_spaces(s)?;
    let (s, ident) = parse_ident(s)?;
    let (s, _) = tag("(")(s)?;
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
                let (ss, _) = tag(",")(s)?;
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
            location,
            ident: ident.trim(),
            out_typ,
            args,
            body,
        },
    ))
}

fn parse_class_attributes(s: Span) -> IResult<Span, ESClassAttributes> {
    let mut s: Span = s;
    let mut attributes: Vec<ESDeclare> = Vec::new();
    while let Ok((ss, attribute)) = parse_declaration(s) {
        s = ss;
        attributes.push(attribute);
    }
    Ok((s, attributes))
}

fn parse_class(s: Span) -> IResult<Span, ESClass> {
    let (s, _) = tag("class")(s)?;
    let (s, pos) = position(s)?;
    let (s, _) = take_spaces(s)?;
    let (s, ident) = parse_ident(s)?;
    let (s, _) = take_spaces(s)?;
    let (s, _) = tag("{")(s)?;
    let (s, attributes) = parse_class_attributes(s)?;
    // TODO:  parse body
    let (s, _) = tag("}")(s)?;

    Ok((
        s,
        ESClass {
            location: pos,
            ident: ident.trim(),
            attributes,
        },
    ))
}

#[cfg(test)]
mod tests {

    use nom::IResult;
    use nom_locate::LocatedSpan;

    use crate::es::{ast::{ESDeclare, ESLiteral, ESType, ESAsign, ESString}, parser::parse_asignment};

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
                    location: s,
                    ident: "a",
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
                    location: s,
                    ident: "a",
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
                    location: s,
                    ident: "a",
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
                    location: s,
                    ident: "a",
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
                    location: s,
                    ident: "a",
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
                    location: s,
                    ident: "a",
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
                    location: s,
                    ident: "a",
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
                    location: s,
                    ident: "a",
                    value: ESLiteral::String(ESString { location: unsafe { LocatedSpan::new_from_raw_offset(2, 1, "\"hello\"", ()) }, value: "hello"})
                }
            ))
        );
    }
}
