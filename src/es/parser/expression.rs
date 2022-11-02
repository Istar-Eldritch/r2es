use std::cell::RefCell;

use nom::{bytes::{streaming::tag, complete::take_while}, character::{is_digit, is_alphanumeric}};

use crate::es::{ast::{Span, ESLiteral, ESExpression, ESUnaryOp, ESUnary, ESFactorOp, ESFactor, ESTermOp, ESTerm, ESCmpOp, ESCmp, ESEqOp, ESEq, ESGroup}, parser::take_spaces};

use super::{ESError, parse_ident};

fn parse_bool(s: Span) -> Result<(Span, bool), ESError> {
    if let Ok((s, _)) = tag::<&str, Span, ()>("true")(s) {
        Ok((s, true))
    } else if let Ok((s, _)) = tag::<&str, Span, ()>("false")(s) {
        Ok((s, false))
    } else {
        Err(ESError::InvalidInput(s, "Boolean"))
    }
}

fn parse_int(s: Span) -> Result<(Span, u64), ESError> {
    let (s, r) = take_while::<_, _, ()>(|ch: char| ch.is_ascii() && is_digit(ch as u8) || ch == '.')(s).map_err(|_| ESError::Unexpected)?;
    r.trim()
        .parse::<u64>()
        .map(|v| ((s, v)))
        .map_err(|_| ESError::InvalidInput(s, "Int"))
}

fn parse_float(s: Span) -> Result<(Span, f64), ESError> {
    let (s, r) = take_while::<_, _, ()>(|ch: char| ch.is_ascii() && (is_digit(ch as u8) || ch == '.'))(s).map_err(|_| ESError::Unexpected)?;
    r.trim()
        .parse::<f64>()
        .map(|v| ((s, v)))
        .map_err(|_| ESError::InvalidInput(s, "Float"))
}

fn parse_str(s: Span) -> Result<(Span, String), ESError> {
    let (s, quoted) = take_while::<_, _, ()>(|ch: char| ch.is_ascii() && (is_alphanumeric(ch as u8) || ch == '"'))(s).map_err(|_| ESError::Unexpected)?;
    let (value, _) = tag::<_,_,()>("\"")(quoted).map_err(|_| ESError::Unexpected)?;
    let value = take_while::<_, _, ()>(|ch: char| ch.is_ascii() && is_alphanumeric(ch as u8))(value);
    let (pending, r) = (value).map(
        |(s, inner)| {
            (
                s,
                String::from(inner.trim()),
            )
        },
    ).map_err(|_| ESError::Unexpected)?;
    tag::<_,_,()>("\"")(pending).map_err(|_| ESError::Unexpected)?;
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
    } else if let Ok((s, r)) = parse_ident(s) {
        Ok((s, ESLiteral::Ident(r.trim().into())))
    } else {
        Err(ESError::InvalidInput(s, "Literal"))
    }
}

fn parse_literal_or_group(s: Span) -> Result<(Span, ESExpression), ESError> {
        if let Ok((s, parsed)) = parse_literal(s) {
        Ok((s, ESExpression::Literal(parsed)))
    } else if let Ok((s, parsed)) = parse_group(s) {
        Ok((s, ESExpression::Group(Box::new(parsed))))
    } else {
        Err(ESError::InvalidInput(s, "Not valid primary"))
    }
}

fn parse_unary(s: Span) -> Result<(Span, ESExpression), ESError> {
    let (ss, op) = take_while::<_,_,()>(|ch| ch == '-' || ch == '!')(s).map_err(|_| ESError::InvalidInput(s, "UnaryOperator"))?;
    let op = match op.trim() {
        "-" => ESUnaryOp::Negate,
        "!" => ESUnaryOp::LogicalNegate,
        _ => return parse_literal_or_group(s)
    };

    let (s, expr) = parse_literal_or_group(ss)?;
    Ok((s, ESExpression::Unary(Box::new(ESUnary { op, expr }))))
}

fn parse_factor(s: Span) -> Result<(Span, ESExpression), ESError> {
    let is_factor_op= |ch| ch == '*' || ch == '/';
    let (s, left) = parse_unary(s)?;
    let (s, _) = take_spaces(s)?;
    let mut s = s;
    let mut left = left;

    while let Ok((ss, op)) = take_while::<_,_,()>(is_factor_op)(s).map_err(|_| ESError::InvalidInput(s, "FactorOperator")) {
        let op = match op.trim() {
            "*" => ESFactorOp::Multiply,
            "/" => ESFactorOp::Divide,
            _ => return Ok((s,left))
        };
        let (ss, _) = take_spaces(ss)?;
        let (ss, right) = parse_unary(ss)?;
        let (ss, _) = take_spaces(ss)?;
        s = ss;
        left = ESExpression::Factor(Box::new(ESFactor { op, left, right }))
    }
    Ok((s, left))
}

fn parse_term(s: Span) -> Result<(Span, ESExpression), ESError> {
    let is_term_op= |ch| ch == '+' || ch == '-';
    let (s, left) = parse_factor(s)?;
    let mut left = left;
    let (s, _) = take_spaces(s)?;
    let mut s = s;
    while let Ok((ss, op)) = take_while::<_,_,()>(is_term_op)(s).map_err(|_| ESError::InvalidInput(s, "TermOperator")) {
        let op = match op.trim() {
            "+" => ESTermOp::Add,
            "-" => ESTermOp::Substract,
            _ => return Ok((s, left))
        };
        let (ss, _) = take_spaces(ss)?;
        let (ss, right) = parse_factor(ss)?;
        let (ss, _) = take_spaces(ss)?;
        s = ss;
        left = ESExpression::Term(Box::new(ESTerm { op, left, right }))
    }

    Ok((s, left))
}

fn parse_cmp(s: Span) -> Result<(Span, ESExpression), ESError> {
    let is_cmp_op= |ch| ch == '>' || ch == '<' || ch == '=';
    let (s, left) = parse_term(s)?;
    let mut left = left;
    let (s, _) =  take_spaces(s)?;
    let mut s = s;
    while let Ok((ss, op)) = take_while::<_,_,()>(is_cmp_op)(s).map_err(|_| ESError::InvalidInput(s, "CmpOperator")) {
        let op = match op.trim() {
            ">" => ESCmpOp::Gt,
            "<" => ESCmpOp::Lt,
            ">=" => ESCmpOp::Gte,
            "<=" => ESCmpOp::Lte,
            _ => return Ok((s, left))
        };
        let (ss, _) = take_spaces(ss)?;
        let (ss, right) = parse_term(ss)?;
        let (ss, _) = take_spaces(ss)?;
        s = ss;
        left = ESExpression::Cmp(Box::new(ESCmp { op, left, right }))
    }
    Ok((s, left))
}

fn parse_eq(s: Span) -> Result<(Span, ESExpression), ESError> {
    let is_eq_op= |ch| ch == '=' || ch == '!';
    let (s, left) = parse_cmp(s)?;
    let (s, _) = take_spaces(s)?;

    let mut left = left;
    let mut s = s;

    while let Ok((ss, op)) = take_while::<_,_,()>(is_eq_op)(s).map_err(|_| ESError::InvalidInput(s, "EqOperator")) {
        let op = match op.trim() {
            "==" => ESEqOp::Eq,
            "!=" => ESEqOp::Ne,
            _ => return Ok((s, left))
        };
        let (ss, _) = take_spaces(ss)?;
        let (ss, right) = parse_cmp(ss)?;
        let (ss, _) = take_spaces(ss)?;
        s = ss;
        left = ESExpression::Eq(Box::new(ESEq { op, left, right }))
    }

    Ok((s, left))
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
        Ok((s, ESGroup { inner }))
}

pub fn parse_expression(s: Span) -> Result<(Span, ESExpression), ESError> {
    let (s, _) = take_spaces(s)?;
    if s.is_empty() {
        return Err(ESError::EmptyInput(s))
    }
    parse_eq(s)
}


#[cfg(test)]
mod tests {
    use nom_locate::LocatedSpan;

    use crate::es::{ast::{ ESLiteral, ESGroup, ESExpression, ESTerm, ESTermOp, ESFactor, ESFactorOp}};

    use super::*;

    #[test]
    fn parses_term_1() {
        let s = LocatedSpan::from("1 + 2");
        let dec = parse_term(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(5, 1, "", ()) },
                ESExpression::Term(Box::new(ESTerm {
                    left: ESExpression::Literal(ESLiteral::Int(1)),
                    right: ESExpression::Literal(ESLiteral::Int(2)),
                    op: ESTermOp::Add
                }))
            ))
        );
    }

    #[test]
    fn parses_terms_2() {

        let s = LocatedSpan::from("1 - 2");
        let dec = parse_term(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(5, 1, "", ()) },
                ESExpression::Term(Box::new(ESTerm {
                    left: ESExpression::Literal(ESLiteral::Int(1)),
                    right: ESExpression::Literal(ESLiteral::Int(2)),
                    op: ESTermOp::Substract
                }))
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
                        right: crate::es::ast::ESExpression::Literal(ESLiteral::Int(3)),
                        op: crate::es::ast::ESTermOp::Add
                    }))
                    
                }
            ))
        );
    }

    #[test]
    fn parses_literal_1() {
        let s = LocatedSpan::from("1");
        let dec = parse_literal(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(1,1,"",())},
                ESLiteral::Int(1)
            ))
        )
    }

    #[test]
    fn parses_literal_2() {
        let s = LocatedSpan::from("1.2");
        let dec = parse_literal(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(3,1,"",())},
                ESLiteral::Float(1.2)
            ))
        )
    }


    #[test]
    fn parses_literal_3() {
        let s = LocatedSpan::from("true");
        let dec = parse_literal(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(4,1,"",())},
                ESLiteral::Bool(true)
            ))
        );

        let s = LocatedSpan::from("false");
        let dec = parse_literal(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(5,1,"",())},
                ESLiteral::Bool(false)
            ))
        )
    }


    #[test]
    fn parses_literal_4() {
        let s = LocatedSpan::from("\"hello\"");
        let dec = parse_literal(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(7,1,"",())},
                ESLiteral::String("hello".into())
            ))
        );

        let s = LocatedSpan::from("false");
        let dec = parse_literal(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(5,1,"",())},
                ESLiteral::Bool(false)
            ))
        )
    }


    #[test]
    fn parses_factor_1() {
        let s = LocatedSpan::from("2*1");
        let dec = parse_factor(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(3,1,"",())},
                ESExpression::Factor(Box::new(ESFactor {
                    op: ESFactorOp::Multiply,
                    left: ESExpression::Literal(ESLiteral::Int(2)),
                    right: ESExpression::Literal(ESLiteral::Int(1))
                }
            ))))
        );
    }

    #[test]
    fn parses_factor_2() {
        let s = LocatedSpan::from("2/1");
        let dec = parse_factor(s);
        assert_eq!(
            dec,
            Ok((
                unsafe { LocatedSpan::new_from_raw_offset(3,1,"",())},
                ESExpression::Factor(Box::new(ESFactor {
                    op: ESFactorOp::Divide,
                    left: ESExpression::Literal(ESLiteral::Int(2)),
                    right: ESExpression::Literal(ESLiteral::Int(1))
                }
            ))))
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
