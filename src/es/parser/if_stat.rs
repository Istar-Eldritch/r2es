use nom::bytes::complete::tag;

use crate::es::ast::{Span, ESIf, ESIfBranch, ESExpression, ESLiteral};

use super::{ESError, take_spaces, expression::parse_expression, parse_block};


pub fn parse_if(s: Span) -> Result<(Span, ESIf), ESError> {
    let (s, _) = take_spaces(s)?;
    let (s, _) = tag::<_,_,()>("if")(s).map_err(|_| ESError::InvalidInput(s, "if"))?;
    let (s, _) =take_spaces(s)?;
    let (s, _) = tag::<_,_,()>("(")(s).map_err(|_| ESError::InvalidInput(s, "("))?;
    let (s, conditional) = parse_expression(s)?;
    let (s, _) = tag::<_,_,()>(")")(s).map_err(|_| ESError::InvalidInput(s, ")"))?;
    let (s, _) =take_spaces(s)?;

    let (s, body) = parse_block(s)?;
    let mut s = s;
    let mut branches = vec![ESIfBranch { conditional, body }];
    while let Ok((ss, _)) = tag::<_,_, ()>("else if")(s) {
        let (ss, conditional) =parse_expression(ss)?;
        let (ss, body) = parse_block(ss)?;
        s = ss;
        branches.push(ESIfBranch { conditional, body });
    }

    (s, _) = take_spaces(s)?;

    if let Ok((ss, _)) = tag::<_,_,()>("else")(s) {
        let (ss, body) = parse_block(ss)?;
        s = ss;
        branches.push(ESIfBranch { conditional: ESExpression::Literal(ESLiteral::Bool(true)) , body })
    }
    
    Ok((s, ESIf { branches }))
}

#[cfg(test)]
mod tests {
    use nom_locate::LocatedSpan;

    use crate::es::ast::{ESBlock, ESStatement};

    use super::*;



    #[test]
    fn parse_if_1() {
        let s = LocatedSpan::from("
            if(true) {
            }");

        let dec = parse_if(s);

        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(37, 3, "", ())},
                ESIf {
                    branches: vec![ESIfBranch {body: ESBlock { statements: vec![] }, conditional: ESExpression::Literal(ESLiteral::Bool(true)) }]
                }
            ))
        )
    }

    #[test]
    fn parse_if_2() {
        let s = LocatedSpan::from("
            if(true) {
            } else {}");

        let dec = parse_if(s);

        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(45, 3, "", ())},
                ESIf {
                    branches: vec![
                        ESIfBranch {body: ESBlock { statements: vec![] }, conditional: ESExpression::Literal(ESLiteral::Bool(true)) },
                        ESIfBranch {body: ESBlock { statements: vec![] }, conditional: ESExpression::Literal(ESLiteral::Bool(true)) }
                    ]
                }
            ))
        )
    }

    #[test]
    fn parse_if_3() {
        let s = LocatedSpan::from("
            if(true) {
                if(true) {

                }
            } else {}");

        let dec = parse_if(s);

        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(91, 6, "", ())},
                ESIf {
                    branches: vec![
                        ESIfBranch {
                            body: ESBlock {
                                statements: vec![
                                    ESStatement::If(Box::new(ESIf{
                                        branches: vec![
                                            ESIfBranch {
                                                body: ESBlock { statements: vec![] },
                                                conditional: ESExpression::Literal(ESLiteral::Bool(true))
                                            }
                                        ]
                                    }))
                                ]
                            },
                            conditional: ESExpression::Literal(ESLiteral::Bool(true))
                        },
                        ESIfBranch {
                            body: ESBlock { statements: vec![] },
                            conditional: ESExpression::Literal(ESLiteral::Bool(true))
                        }
                    ]
                }
            ))
        )
    }
}