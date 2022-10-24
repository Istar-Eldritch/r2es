use std::str::from_utf8;

use nom::bytes::complete::{tag, take_until, take_while};
use nom::character::{is_alphabetic, is_space};
use nom::IResult;
use nom_locate::{position, LocatedSpan};

use super::ast::{ESClass, ESClassAttributes, ESDeclare, ESFn, ESStatement, Item, Span};

fn parse_function(s: Span) -> IResult<Span, ESFn> {
    unimplemented!()
}

fn parse_declaration(s: Span) -> IResult<Span, ESDeclare> {
    let (s, _) = take_while(|c: char| c.is_ascii() && is_space(c as u8))(s)?;
    let mut cst = false;
    let mut s = s;
    if let Ok((ss, _)) = tag::<&str, LocatedSpan<&str>, ()>("const")(s) {
        cst = true;
        s = ss;
    }

    unimplemented!()
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
    let (s, _) = take_while(|c: char| c.is_ascii() && is_space(c as u8))(s)?;
    let (s, ident) = take_while(|c: char| c.is_ascii() && is_alphabetic(c as u8))(s)?;
    let (s, _) = take_while(|c: char| c.is_ascii() && is_space(c as u8))(s)?;
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
