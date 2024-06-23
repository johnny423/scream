use pest::iterators::Pair;
use pest_derive::Parser;
use anyhow::Context;
use pest::Parser;
use crate::expr::Expr;

#[derive(Parser)]
#[grammar = "lang.pest"]
struct LangParser;

fn parse(pair: Pair<Rule>) -> anyhow::Result<Expr> {
    match pair.as_rule() {
        Rule::expr => parse(
            pair.into_inner()
                .next()
                .context("Can't fail - expression always has internal")?,
        ),
        Rule::number => Ok(Expr::Num(pair.as_str().into())),
        Rule::boolean => Ok(Expr::Bool(pair.as_str().into())),
        Rule::identifier => Ok(Expr::Var(pair.as_str().into())),
        Rule::or_expr => {
            let mut inner = pair.into_inner();
            let lhs = parse(inner.next().expect("Or must have two inner expressions"))?;
            let rhs = parse(inner.next().expect("Or must have two inner expressions"))?;
            Ok(Expr::Or(Box::new(lhs), Box::new(rhs)))
        }
        Rule::and_expr => {
            let mut inner = pair.into_inner();
            let lhs = parse(inner.next().expect("And must have two inner expressions"))?;
            let rhs = parse(inner.next().expect("And must have two inner expressions"))?;
            Ok(Expr::And(Box::new(lhs), Box::new(rhs)))
        }
        Rule::if_expr => {
            let mut inner_rule = pair.into_inner();
            let pred = parse(
                inner_rule
                    .next()
                    .expect("If must have two inner expressions"),
            )?;
            let then = parse(
                inner_rule
                    .next()
                    .expect("If must have two inner expressions"),
            )?;
            let otherwise = parse(
                inner_rule
                    .next()
                    .expect("If must have two inner expressions"),
            )?;
            Ok(Expr::If {
                pred: Box::new(pred),
                then: Box::new(then),
                otherwise: Box::new(otherwise),
            })
        }
        Rule::let_expr => {
            let mut inner_rule = pair.into_inner().peekable();
            let mut vars = vec![];
            let mut vals = vec![];
            for p in inner_rule
                .clone()
                .take_while(|p| p.as_rule() == Rule::binding)
            {
                let mut binding = p.into_inner();
                vars.push(binding.next().unwrap().as_str().into());
                vals.push(parse(binding.next().unwrap())?);
            }

            let pair2 = inner_rule.find(|p| p.as_rule() == Rule::expr).unwrap();
            let body = parse(pair2)?;
            Ok(Expr::Let {
                vars,
                vals,
                body: Box::new(body),
            })
        }
        Rule::lambda => {
            let mut inner_rule = pair.into_inner();
            let args: Vec<_> = inner_rule
                .clone()
                .take_while(|p| p.as_rule() == Rule::identifier)
                .map(|p| p.as_str().into())
                .collect();

            let body = inner_rule.find(|p| p.as_rule() == Rule::expr).unwrap();
            Ok(Expr::Proc {
                args,
                body: Box::new(parse(body)?),
            })
        }
        Rule::procedure_call => {
            let mut inner_rule = pair.into_inner();
            let operator = parse(inner_rule.next().unwrap())?;
            let operands = inner_rule.map(parse).collect::<anyhow::Result<_>>()?;
            Ok(Expr::App(Box::new(operator), operands))
        }
        _ => {
            println!("AHHHHHH got {:?}", pair.as_rule());
            panic!()
        }
    }
}

pub fn parse_program(input: &str) -> Expr {
    let a = LangParser::parse(Rule::expr, input)
        .expect("parsing failed")
        .next()
        .unwrap();
    let b = parse(a).unwrap();
    b
}
