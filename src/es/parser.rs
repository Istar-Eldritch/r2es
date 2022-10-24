use std::str::from_utf8;

use nom::bytes::complete::{tag, take_until, take_while};
use nom::character::{is_alphabetic, is_space, is_digit, is_alphanumeric};
use nom::IResult;
use nom_locate::{position, LocatedSpan};
use syn::token;

use super::ast::{ESClass, ESClassAttributes, ESDeclare, ESFn, ESStatement, Item, Span, ESType, ESLiteral, ESString};

fn take_spaces(s: Span) -> IResult<Span, Span> {
    take_while(|c: char| c.is_ascii() && is_space(c as u8))(s)
}

fn parse_function(s: Span) -> IResult<Span, ESFn> {
    unimplemented!()
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
        _ => Err(nom::Err::Error(nom::error::Error::new(s, nom::error::ErrorKind::IsNot)))

    }
}

fn parse_ident(s: Span) -> IResult<Span, Span> {
    if s.len() == 0 {
        return Err(nom::Err::Error(nom::error::Error::new(s, nom::error::ErrorKind::Fail)))
    }
    take_while(|c: char| c.is_ascii() && is_alphabetic(c as u8))(s)
}

fn parse_bool(s: Span) -> IResult<Span, bool> {
    if let Ok((s, _)) = tag::<&str, Span, ()>("true")(s) {
        Ok((s, true))
    } else if let Ok((s, _)) = tag::<&str, Span, ()>("false")(s) {
        Ok((s, false))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(s, nom::error::ErrorKind::IsNot)))
    }
}

fn parse_int(s: Span) -> IResult<Span, u64> {
    let (s, r) = take_while(|ch: char| ch.is_ascii() && is_digit(ch as u8))(s)?;
    r.trim().parse::<u64>()
        .map(|v| ((s, v)))
        .map_err(|_| nom::Err::Error(nom::error::Error::new(s, nom::error::ErrorKind::IsNot)))
}

fn parse_float(s: Span) -> IResult<Span, f64> {
    let (s, r) = take_while(|ch: char| ch.is_ascii() && is_digit(ch as u8) || ch == '.')(s)?;
    r.trim().parse::<f64>()
        .map(|v| ((s, v)))
        .map_err(|_| nom::Err::Error(nom::error::Error::new(s, nom::error::ErrorKind::IsNot)))
}

fn parse_str(s: Span) -> IResult<Span, ESString> {
    let (s, _) = tag("\"")(s)?;
    let (s, r) = take_while(|ch: char| ch.is_ascii() && is_alphanumeric(ch as u8))(s)
        .map(|(s, value)| (s, ESString {location: value, value: value.trim() }))?;
    let (s, _) = tag("\"")(s)?;
    Ok((s, r))
}

fn parse_literal(typ: ESType, s: Span) ->  IResult<Span, ESLiteral> {
    if let Ok((s, r)) = parse_bool(s) {
        Ok((s, ESLiteral::Bool(r)))
    } else if let Ok((s, r)) = parse_int(s) {
        Ok((s, ESLiteral::Int(r)))
    } else if let Ok((s, r)) = parse_str(s) {
        Ok((s, ESLiteral::String(r)))
    } else if let Ok((s, r)) = parse_float(s) {
        Ok((s, ESLiteral::Float(r)))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(s, nom::error::ErrorKind::IsNot)))
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
        let (ss, v) = parse_literal(typ.clone(), ss)?;
        s = ss;
        value = Some(v); 
    }

    let (s, _) = tag(";")(s)?;

    Ok((s, ESDeclare {
        location,
        ident: ident.trim(),
        constant,
        typ,
        value
    }))
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

    use crate::es::ast::{ESDeclare, ESLiteral, ESType};

    use super::{parse_declaration, parse_ident};

    #[test]
    fn parses_identities() {
        let mut s = LocatedSpan::from("");
        let dec = parse_ident(s);
        assert!(dec.is_err());
        s = LocatedSpan::from("abc");
        let dec = parse_ident(s);
        assert_eq!(dec, Ok((unsafe {LocatedSpan::new_from_raw_offset(3, 1, "", ())}, s)));
        s = LocatedSpan::from("abc;");
        let dec = parse_ident(s);
        assert_eq!(dec, Ok((
            unsafe {LocatedSpan::new_from_raw_offset(3, 1, ";", ())},
            unsafe {LocatedSpan::new_from_raw_offset(0, 1, "abc", ())}
        )));
    }

    #[test]
    fn parses_declarations() {
        let mut s = LocatedSpan::from("");
        let dec = parse_declaration(s);
        assert!(dec.is_err());
        s = LocatedSpan::from("int a;");
        let dec = parse_declaration(s);
        assert_eq!(
            dec,
            Ok((
                unsafe {LocatedSpan::new_from_raw_offset(6, 1, "", ())},
                ESDeclare {location: s, ident: "a", constant: false, typ: ESType::Int, value: None }))
        );

        s = LocatedSpan::from("const int a;");
        let dec = parse_declaration(s);
        assert_eq!(
            dec,
            Ok((
                unsafe {LocatedSpan::new_from_raw_offset(12, 1, "", ())},
                ESDeclare {location: s, ident: "a", constant: true, typ: ESType::Int, value: None }))
        );

        s = LocatedSpan::from("const int a = 1;");
        let dec = parse_declaration(s);
        assert_eq!(
            dec,
            Ok((
                unsafe {LocatedSpan::new_from_raw_offset(16, 1, "", ())},
                ESDeclare {location: s, ident: "a", constant: true, typ: ESType::Int, value: Some(ESLiteral::Int(1)) }))
        );
    }
}