use nom::bytes::complete::tag;

use crate::es::ast::{Span, ESFnArg, ESFn};

use super::{take_spaces, ESError, parse_type, parse_ident, parse_block};

fn parse_function_arg(s: Span) -> Result<(Span, ESFnArg), ESError> {
    let (s, _) = take_spaces(s)?;
    let (s, typ) = parse_type(s)?;
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

pub fn parse_function_def(s: Span) -> Result<(Span, ESFn), ESError> {
    let (s, _) = take_spaces(s)?;
    let (s, out_typ) = parse_type(s)?;
    let (s, _) = take_spaces(s)?;
    let (s, ident) = parse_ident(s)?;
    let (s, _) = tag::<_,_,()>("(")(s).map_err(|_| ESError::InvalidInput(s, "("))?;
    let mut s = s;
    let mut args: Vec<ESFnArg> = Vec::new();
    let mut closing_tag: Result<(Span, Span), ESError>;
    let mut first = true;
    loop {
        let (ss, _) = take_spaces(s)?;
        s = ss;
        closing_tag = tag::<_,_,()>(")")(s).map_err(|_| ESError::InvalidInput(s, ")"));
        if let Ok((ss, _)) = closing_tag {
            s = ss;
            break;
        } else {
            if !first {
                let (ss, _) = tag::<_,_,()>(",")(s).map_err(|_| ESError::InvalidInput(s, ","))?;
                let (ss, _) = take_spaces(ss)?;
                s = ss;
            }
            let (ss, arg) = parse_function_arg(s)?;
            first = false;
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

#[cfg(test)]
mod tests {
    use std::vec;

    use nom_locate::LocatedSpan;

    use crate::es::ast::{ESBlock, ESType, ESStatement, ESDeclare, ESExpression, ESLiteral};

    use super::*;

    #[test]
    fn parse_function_def_0() {
        let s = LocatedSpan::from("void Test() {}");
        let dec = parse_function_def(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(14, 1, "", ()) },
                ESFn {args: vec![], body: ESBlock {statements: vec![]}, ident: "Test".into(), out_typ: ESType::Void}
            ))
        );
    }

    #[test]
    fn parse_function_def_1() {
        let s = LocatedSpan::from("void Test(int a, float b) {}");
        let dec = parse_function_def(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(28, 1, "", ()) },
                ESFn {
                    args: vec![
                        ESFnArg {by_ref: false, default: None, ident: "a".into(), typ: ESType::Int},
                        ESFnArg {by_ref: false, default: None, ident: "b".into(), typ: ESType::Float},  
                    ],
                    body: ESBlock {statements: vec![]}, ident: "Test".into(),
                    out_typ: ESType::Void
                }
            ))
        );
    }


    #[test]
    fn parse_function_def_2() {
        let s = LocatedSpan::from("
            void Test(int a, float b) {
                int s = a;
            }");
        let dec = parse_function_def(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(81, 4, "", ()) },
                ESFn {
                    args: vec![
                        ESFnArg {by_ref: false, default: None, ident: "a".into(), typ: ESType::Int},
                        ESFnArg {by_ref: false, default: None, ident: "b".into(), typ: ESType::Float},  
                    ],
                    body: ESBlock {statements: vec![
                        ESStatement::Declare(ESDeclare {constant: false, ident: "s".into(), typ: ESType::Int, value: Some(ESExpression::Literal(ESLiteral::Ident("a".into())))})
                    ]}, ident: "Test".into(),
                    out_typ: ESType::Void
                }
            ))
        );
    }

    #[test]
    fn parse_function_def_3() {
        let s = LocatedSpan::from("
            void Test(int a, float b) {
                return a;
            }");
        let dec = parse_function_def(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(80, 4, "", ()) },
                ESFn {
                    ident: "Test".into(),
                    args: vec![
                        ESFnArg {by_ref: false, default: None, ident: "a".into(), typ: ESType::Int},
                        ESFnArg {by_ref: false, default: None, ident: "b".into(), typ: ESType::Float},  
                    ],
                    body: ESBlock {
                        statements: vec![
                            ESStatement::Return(ESExpression::Literal(ESLiteral::Ident("a".into())))
                        ]
                    },
                    out_typ: ESType::Void
                }
            ))
        );
    }
}