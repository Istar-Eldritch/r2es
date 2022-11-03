use nom::bytes::complete::tag;

use crate::es::ast::*;

use super::{ESError, take_spaces, parse_statement, expression::parse_expression, parse_block};



pub fn parse_for_loop(s: Span) -> Result<(Span, ESStatement), ESError> {
    let (s, _) = take_spaces(s)?;
    let (s, _) = tag::<_,_,()>("for")(s).map_err(|_| ESError::InvalidInput(s, "for"))?;
    let (s, _) = take_spaces(s)?;
    let (s, _) = tag::<_,_,()>("(")(s).map_err(|_| ESError::InvalidInput(s, "("))?;
    let (s, initexpr) = parse_statement(s)?;
    let (s, condition) = parse_statement(s)?;
    let (s, endexpr) = parse_expression(s)?;
    let (s, _) = tag::<_,_,()>(")")(s).map_err(|_| ESError::InvalidInput(s, ")"))?;
    let (s, _) = take_spaces(s)?;
    let (s, body) = parse_block(s)?;
    Ok((s, ESStatement::For(Box::new(ESFor { initexpr, condition, endexpr, body }))))
}

#[cfg(test)]
mod tests {
    use nom_locate::LocatedSpan;

    use crate::es::ast::*;

    use super::parse_for_loop;

    #[test]
    fn parse_for_0() {
        let s = LocatedSpan::from("for(int a = 1;a < 10;a++) {}");
        let dec = parse_for_loop(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(28,1,"",())},
                ESStatement::For(Box::new(ESFor {
                    initexpr: ESStatement::Declare(ESDeclare { ident: "a".into(), constant: false, typ: ESType::Int, value: Some(ESExpression::Literal(ESLiteral::Int(1))) }),
                    condition: ESStatement::Expression(ESExpression::Cmp(Box::new(ESCmp {
                        left: ESExpression::Literal(ESLiteral::Ident("a".into())),
                        op: ESCmpOp::Lt,
                        right: ESExpression::Literal(ESLiteral::Int(10))
                    }))),
                    endexpr: ESExpression::Suffix(ESAddSufix { ident: "a".into() }),
                    body: ESBlock { statements: vec![] }
                }))
            ))
        )
    }
}