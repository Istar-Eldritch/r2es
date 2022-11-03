use nom::bytes::complete::tag;

use crate::es::ast::{Span, ESClassAttributes, ESDeclare, ESFn, ESClass};

use super::{ESError, parse_declaration, function::parse_function_def, take_spaces, parse_ident};


fn parse_class_attributes(s: Span) -> Result<(Span, ESClassAttributes), ESError> {
    let mut s: Span = s;
    let mut attributes: Vec<ESDeclare> = Vec::new();
    while let Ok((ss, attribute)) = parse_declaration(s) {
        if let Ok((ss, _)) = tag::<_,_,()>(";")(ss) {
            s = ss;
            attributes.push(attribute);
        } else {
            break;
        }
    }
    Ok((s, attributes))
}

fn parse_class_methods(s: Span) -> Result<(Span, Vec<ESFn>), ESError> {
    let mut s =s;
    let mut methods = Vec::new();
    while let Ok((ss, method)) = parse_function_def(s) {
        s = ss;
        methods.push(method);
    }
    Ok((s, methods))
}

pub fn parse_class(s: Span) -> Result<(Span, ESClass), ESError> {
    let (s, _) = take_spaces(s)?;
    let (s, _) = tag::<_,_,()>("class")(s).map_err(|_| ESError::InvalidInput(s, "class"))?;
    let (s, _) = take_spaces(s)?;
    let (s, ident) = parse_ident(s)?;
    let (s, _) = take_spaces(s)?;
    let (s, _) = tag::<_,_,()>("{")(s).map_err(|_| ESError::InvalidInput(s, "{"))?;
    let (s, attributes) = parse_class_attributes(s)?;

    let (s, methods) = parse_class_methods(s)?;
    let (s, _) = take_spaces(s)?;
    // TODO:  parse body
    let (s, _) = tag::<_,_,()>("}")(s).map_err(|_| ESError::InvalidInput(s, "}"))?;

    Ok((
        s,
        ESClass {
            ident: String::from(ident.trim()),
            attributes,
            methods
        },
    ))
}

#[cfg(test)]
mod tests {
    use nom_locate::LocatedSpan;
    use super::*;
    use crate::es::ast::*;

    #[test]
    fn parse_class_0() {
        let s = LocatedSpan::from("class Test {}");
        let dec = parse_class(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(13, 1, "", ()) },
                ESClass { ident: "Test".into(), attributes: vec![], methods: vec![]}
            ))
        );
    }

    #[test]
    fn parse_class_1() {
        let s = LocatedSpan::from("
            class Test {
                int a = 3;
            }");
        let dec = parse_class(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(66, 4, "", ()) },
                ESClass { ident: "Test".into(), attributes: vec![
                    ESDeclare { ident: "a".into(), constant: false, typ: ESType::Int, value: Some(ESExpression::Literal(ESLiteral::Int(3)))}
                ], methods: vec![]}
            ))
        );
    }


    #[test]
    fn parse_class_2() {
        let s = LocatedSpan::from("
            class Test {
                int a = 3;
                void test() {}
            }");
        let dec = parse_class(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(97, 5, "", ()) },
                ESClass { ident: "Test".into(),
                    attributes: vec![
                        ESDeclare { ident: "a".into(), constant: false, typ: ESType::Int, value: Some(ESExpression::Literal(ESLiteral::Int(3)))}
                    ],
                    methods: vec![
                        ESFn {args: vec![], body: ESBlock { statements: vec![] }, ident: "test".into(), out_typ: ESType::Void}
                    ]
                }
            ))
        );
    }
}