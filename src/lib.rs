//! Binary Lambda Calculus Playground
//!
//! This module defines a representation of λ-calculus and
//! provides parsers and printers for it.  The aim of this module
//! is to enable experimentation with the binary encoding described
//! by John Tromp, see [BLC](https://en.wikipedia.org/wiki/Binary_combinatory_logic)
//! and [Diagrams](https://tromp.github.io/cl/diagrams.html)
//!
//! Encoding
//! ```pseudo
//! <term> ::= 00                      abstraction
//!          | 01 <term> <term>        application
//!          | 1 <var>                 variable
//! <var> ::= 0 | 1 <var>
//! ```

pub mod list;

use list::*;
use std::fmt;
use List::*;
use Term::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// Abstraction wraps a λ-terms in a λ x .
    Ab(Box<Term>),
    /// Application of two λ-terms
    Ap(Box<Term>, Box<Term>),
    /// Variables, de Bruijn encoded (count the number of abstractions)
    Va(usize),
}

const CLASSIC_NAMES: &[u8] = b"xyzwvabcdef";

fn fmt_var(f: &mut fmt::Formatter, level: usize) -> fmt::Result {
    if level < CLASSIC_NAMES.len() {
        write!(f, "{}", CLASSIC_NAMES[level] as char)
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
            Va(n) => write!(f, "v{}", *n - 1),
        }
    }
}

/// λ-terms implement the `Display` trait
impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_level(f, 0)
    }
}

fn parse_var(src: &str) -> Result<(&str, &str), &str> {
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

    Ok((src, ""))
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
        let (body, src) = parse_abs(src, &env.cons(var))?;
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
                e = Ap(Box::new(e), Box::new(t));
            }
            Err(_) => {
                return Ok((e, src));
            }
        }
    }
}

fn parse_term<'a>(src: &'a str, env: &List<&'a str>) -> Result<(Term, &'a str), &'a str> {
    if let Some(src) = parse_match(src, "λ") {
        return parse_abs(src, env);
    }

    if let Some(src) = parse_match(src, "(") {
        let (res, src) = parse_exp(src, env)?;
        match parse_match(src, ")") {
            Some(src) => return Ok((res, src)),
            None => return Err("missing )"),
        }
    }

    let (v, src) = parse_var(src)?;
    let mut e = env;
    let mut n = 0;
    loop {
        if !e.is_empty() {
            if e.head() == &v {
                return Ok((Va(n), src));
            }
            e = e.tail();
            n += 1;
        } else {
            return Err("Unknown variable");
        }
    }
}

/// Parse λ-terms
///
/// The textual language of λ-terms is
///
/// ```grammar
/// var ::= <alpha><alphanum>*
/// exp ::= var | λ var* . exp | exp exp*
/// ```
///
/// # Example
///
/// Use parse a string expression and pretty-print it.
///
/// NOTE: unparsing cannot reconstruct the last variable names, thus the
/// variable names will most likely differ.
/// ```
/// assert_eq!(
///     blc::parse("λ a b . (a λx.b)").unwrap().to_string(),
///     "λx.λy.(x) (λz.y)");
/// ```

pub fn parse<'a>(src: &'a str) -> Result<Term, &'a str> {
    match parse_exp(src, &Nil) {
        Ok((res, src)) if src.trim_start() == "" => Ok(res),
        Ok((_, src)) => Err(src),
        Err(err) => Err(err),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_term() {
        let i = Ab(Box::new(Va(0)));
        assert_eq!(parse_term("λx.x", &Nil), Ok((i.clone(), "")));
        assert_eq!(
            parse_term("λx y.x", &Nil),
            Ok((Ab(Box::new(Ab(Box::new(Va(1))))), ""))
        );
        assert_eq!(parse_term("(λx.x)", &Nil), Ok((i.clone(), "")));
        {
            let x = Box::new(Va(0));
            let ap = Box::new(Ap(x.clone(), x));
            let ab = Ab(ap);
            assert_eq!(&parse_term("λx.x x", &Nil), &Ok((ab.clone(), "")));
            assert_eq!(parse_term("λx.(x) (x)", &Nil), Ok((ab.clone(), "")));
            assert_eq!(parse_term("λx.(x x)", &Nil), Ok((ab.clone(), "")));
            assert_eq!(
                parse_term("  λ  x  .  (  (  x  )  (  x  )  )  ", &Nil),
                Ok((ab.clone(), "  "))
            );
        }
        assert_eq!(
            parse_exp("(λx.x)(λx.x)", &Nil),
            Ok((Ap(Box::new(i.clone()), Box::new(i.clone())), ""))
        );
    }

    fn round_trip(src: &str, expected: &str) -> Result<(), String> {
        let e = parse(src)?;
        assert_eq!(e.to_string(), expected);
        Ok(())
    }

    #[test]
    fn printing() -> Result<(), String> {
        // λf.(λx.x x)(λx.f(x x))
        let i = Ab(Box::new(Va(0)));
        assert_eq!(&i.to_string(), "λx.x");
        assert_eq!(Ab(Box::new(i.clone())).to_string(), "λx.λy.y");

        assert_eq!(
            Ap(Box::new(i.clone()), Box::new(i)).to_string(),
            "(λx.x) (λx.x)"
        );
        round_trip(" λ y . y ", "λx.x")?;
        round_trip("λx.λy.x", "λx.λy.x")?;
        Ok(())
    }

    #[test]
    pub fn main() {
        println!("{:?}", parse("x"));
        println!(
            "{:?}",
            Ap(Box::new(Ab(Box::new(Va(0)))), Box::new(Ab(Box::new(Va(0)))))
        );
        let _primes = "00010001100110010100011010000000010110000010010001010111110111101001000110100001110011010000000000101101110011100111111101111000000001111100110111000000101100000110110";
    }
}
