#![warn(missing_docs)] // warn if there is missing docs
//! Binary Lambda Calculus Playground
//!
//! This module defines a representation of λ-calculus and
//! provides parsers and printers for it.  The aim of this module
//! is to enable experimentation with the binary encoding described
//! by John Tromp, see [https://en.wikipedia.org/wiki/Binary_combinatory_logic][BLC]
//! and [https://tromp.github.io/cl/diagrams.html][Diagrams]
//!
//! Encoding
//! ```
//! <term> ::= 00                      abstraction
//!          | 01 <term> <term>        application
//!          | 1 <var>                 variable
//! <var> ::= 0 | 1 <var>
//! ```

mod list;

use list::*;
use std::fmt;
use std::ops::Sub;
use Term::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Ab(Box<Term>),            // Abstraction
    Ap(Box<Term>, Box<Term>), // Application
    Va(usize),                // de Bruijn encoded variable
}

pub fn ap(t1: Term, t2: Term) -> Term {
    Ap(Box::new(t1), Box::new(t2))
}

impl Sub for Term {
    type Output = Term;
    fn sub(self, other: Term) -> Term {
        ap(self, other)
    }
}

const CUTE: &[u8] = b"xyzwvabcdef";

fn fmt_var(f: &mut fmt::Formatter, level: usize) -> fmt::Result {
    if level < CUTE.len() {
        write!(f, "{}", CUTE[level] as char)
    } else {
        write!(f, "n{}", level)
    }
}

impl Term {
    fn fmt_level(&self, f: &mut fmt::Formatter, level: usize) -> fmt::Result {
        match self {
            Ab(t) => {
                write!(f, "λ",)?;
                fmt_var(f, level)?;
                write!(f, ".",)?;
                t.fmt_level(f, level + 1)
            }
            Ap(t1, t2) => {
                write!(f, "(")?;
                t1.fmt_level(f, level)?;
                write!(f, ") (")?;
                t2.fmt_level(f, level)?;
                write!(f, ")")
            }
            Va(n) if *n < level => fmt_var(f, level - *n - 1),
            Va(n) => write!(f, "v{}? (@ level {})", *n - 1, level),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_level(f, 0)
    }
}

#[test]
fn trim() {
    assert_eq!("  abc  ".trim_start(), "abc  ")
}

fn parse_var<'a>(src: &'a str) -> Result<(&'a str, &'a str), &'a str> {
    let src = src.trim_start();

    if src == "" {
        return Err("EOF");
    }

    for (i, c) in src.char_indices() {
        if i == 0 && !c.is_alphabetic() {
            return Err(src);
        }

        if !c.is_alphanumeric() {
            return Ok((&src[0..i], &src[i..].trim_start()));
        }
    }

    return Ok((src, ""));
}

#[test]
fn test_parse_var() {
    assert_eq!(parse_var(" 0 abc"), Err("0 abc"));
    assert_eq!(parse_var("  foo bar "), Ok(("foo", "bar ")));
}

fn parse_match<'a>(src: &'a str, expected: &str) -> Option<&'a str> {
    let src = src.trim_start();
    if src.starts_with(expected) {
        Some(&src[expected.len()..])
    } else {
        None
    }
}

fn parse_abs<'a>(src: &'a str, env: &List<&'a str>) -> Result<(Term, &'a str), &'a str> {
    let src = src.trim_start();

    if let Ok((var, src)) = parse_var(src) {
        let (body, src) = parse_abs(src, &cons(var, env))?;
        return Ok((Ab(Box::new(body)), src));
    }

    match parse_match(src, ".") {
        Some(src) => parse_exp(src, env),
        None => Err("no ."),
    }
}

// term = λ vars . exp | var | ( exp )
// exp = term | exp term
// a b c === (a @ b) @ c
fn parse_exp<'a>(src: &'a str, env: &List<&'a str>) -> Result<(Term, &'a str), &'a str> {
    let (t, src1) = parse_term(src, env)?;
    let mut e = t;
    let mut src = src1;
    loop {
        match parse_term(src, env) {
            Ok((t, src2)) => {
                src = src2;
                e = ap(e, t);
            }
            Err(_) => {
                return Ok((e, src));
            }
        }
    }
}

fn parse_term<'a>(src: &'a str, env: &List<&'a str>) -> Result<(Term, &'a str), &'a str> {
    match parse_match(src, "λ") {
        Some(src) => return parse_abs(src, env),
        None => {}
    }

    match parse_match(src, "(") {
        Some(src) => {
            let (res, src) = parse_exp(src, env)?;
            match parse_match(src, ")") {
                Some(src) => return Ok((res, src)),
                None => return Err("missing )"),
            }
        }
        None => {}
    }

    let (v, src) = parse_var(src)?;
    let mut e = env;
    let mut n = 0;
    loop {
        match e {
            Some(node) => {
                if node.head == v {
                    return Ok((Va(n), src));
                }
                e = node.tail;
                n += 1;
            }
            None => return Err("Unknown variable"),
        }
    }
}

#[test]
fn test_parse_term() {
    let i = Ab(Box::new(Va(0)));
    assert_eq!(parse_term("λx.x", &None), Ok((i.clone(), "")));
    assert_eq!(
        parse_term("λx y.x", &None),
        Ok((Ab(Box::new(Ab(Box::new(Va(1))))), ""))
    );
    assert_eq!(parse_term("(λx.x)", &None), Ok((i.clone(), "")));
    {
        let x = Box::new(Va(0));
        let ap = Box::new(Ap(x.clone(), x));
        let ab = Ab(ap);
        assert_eq!(&parse_term("λx.x x", &None), &Ok((ab.clone(), "")));
        assert_eq!(parse_term("λx.(x) (x)", &None), Ok((ab.clone(), "")));
        assert_eq!(parse_term("λx.(x x)", &None), Ok((ab.clone(), "")));
        assert_eq!(
            parse_term("  λ  x  .  (  (  x  )  (  x  )  )  ", &None),
            Ok((ab.clone(), "  "))
        );
    }
    assert_eq!(
        parse_exp("(λx.x)(λx.x)", &None),
        Ok((ap(i.clone(), i.clone()), ""))
    );
}

#[cfg(test)]
mod test {
    use super::*;

    fn round_trip(src: &str, expected: &str) -> Result<(), String> {
        let (e, src) = parse_exp(src, &None)?;
        assert_eq!(src.trim_start(), "");
        assert_eq!(e.to_string(), expected);
        Ok(())
    }

    #[test]
    fn printing() -> Result<(), String> {
        // λf.(λx.x x)(λx.f(x x))
        let i = Ab(Box::new(Va(0)));
        assert_eq!(&i.to_string(), "λx.x");
        assert_eq!(Ab(Box::new(i.clone())).to_string(), "λx.λy.y");

        assert_eq!(ap(i.clone(), i).to_string(), "(λx.x) (λx.x)");
        round_trip(" λ y . y ", "λx.x")?;
        round_trip("λx.λy.x", "λx.λy.x")?;
        Ok(())
    }
}

pub fn main() {
    println!("{:?}", parse_exp("x", &None));
    println!(
        "{:?}",
        Ap(Box::new(Ab(Box::new(Va(0)))), Box::new(Ab(Box::new(Va(0)))))
    );
    let _primes = "00010001100110010100011010000000010110000010010001010111110111101001000110100001110011010000000000101101110011100111111101111000000001111100110111000000101100000110110";
}
