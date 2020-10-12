// 20201011 Playground for Binary combinatory logic,
// see https://en.wikipedia.org/wiki/Binary_combinatory_logic
//
// Syntax
// <term> ::= 00 | 01 | 1 <term> <term>
//
// Semantics
//    [ 00 ] == K
//    [ 01 ] == S
//    [ 1 <term1> <term2> ] == ( [<term1>] [<term2>] )
//
// Rewrite rules
//    1100xy  → x
//   11101xyz → 11xz1yz
//
// where x, y, and z are arbitrary subterms.

// ....

// I returns its argument:[4]
//
//     Ix = x
//
// K, when applied to any argument x, yields a one-argument constant function Kx, which, when applied to any argument, returns x:[4]
//
//     Kxy = x
//
// S is a substitution operator. It takes three arguments and then returns the first argument applied to the third, which is then applied to the result of the second argument applied to the third.[4] More clearly:
//
//     Sxyz = xz(yz)
//

use std::fmt;
use std::ops::Sub;

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    K, // \ x y -> x
    S, // \ x y z -> x z (y z)
    A(Box<Term>, Box<Term>),
}

use Term::*;

pub fn ap(t1: Term, t2: Term) -> Term {
    A(Box::new(t1), Box::new(t2))
}

impl Sub for Term {
    type Output = Term;
    fn sub(self, other: Term) -> Term {
        ap(self, other)
    }
}

pub fn k(x: Term, y: Term) -> Term {
    ap(ap(K, x), y)
}
pub fn s(x: Term, y: Term, z: Term) -> Term {
    ap(ap(ap(S, x), y), z)
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            K => write!(f, "K"),
            S => write!(f, "S"),
            A(t1, t2) => {
                write!(f, "{}", t1)?;
                write!(f, " ")?;
                match **t2 {
                    K => write!(f, "K"),
                    S => write!(f, "S"),
                    A(_, _) => write!(f, "({})", t2),
                }
            }
        }
    }
}

fn parse_term(src: &str) -> Result<(Term, &str), &str> {
    match &src[0..2] {
        "00" => Ok((K, &src[2..])),
        "01" => Ok((S, &src[2..])),
        "10" | "11" => {
            let (t1, src) = parse_term(&src[1..])?;
            let (t2, src) = parse_term(&src)?;
            Ok((ap(t1, t2), &src))
        }
        _ => Err(src),
    }
}

fn parse(src: &str) -> Result<Term, &str> {
    match parse_term(src)? {
        (res, "") => Ok(res),
        (_, rest) => Err(rest),
    }
}

#[test]
fn printing() {
    assert_eq!(K.to_string(), "K");
    assert_eq!(S.to_string(), "S");
    assert_eq!(s(K, S, S).to_string(), "S K S S");
    assert_eq!(s(K, ap(K, S), S).to_string(), "S K (K S) S");
}

#[test]
fn unparsing() {
    assert_eq!(parse("00").unwrap(), K);
    assert_eq!(parse("01").unwrap(), S);
    assert_eq!(parse("10001").unwrap().to_string(), "K S");
    assert_eq!(parse("10110001").unwrap().to_string(), "S (K S)");
    assert_eq!((S - (K - S) - K).to_string(), "S (K S) K"); // Curry's "B"
    assert_eq!(
        (S - (S - (K - (S - (K - S) - K)) - S) - (K - K)).to_string(),
        "S (S (K (S (K S) K)) S) (K K)"
    ); // Curry's "C"
}

pub fn main() {
    let primes = "00010001100110010100011010000000010110000010010001010111110111101001000110100001110011010000000000101101110011100111111101111000000001111100110111000000101100000110110";
    println!("Hello, world! {}", ap(S, ap(S, K)));
    for ex in &["00", "01", "10000", "1011010000", "typo", primes] {
        println!("Example {} = {:?}", ex, parse_term(ex));
    }

    // B = S (K S) K
    let b = S - (K - S) - K;
    println!("B = {}", b);
    // C = S (S (K (S (K S) K)) S) (K K)
    // let b = s(k(S), K);
    //    let C = s(s(k(s(k(S), K)), S), k(K));
}

// f x y z = (((f x) y) z) = (@ (@ (@ f x) y) z)
// f-x-y-z
