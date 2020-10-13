#![warn(missing_docs)] // warn if there is missing docs
//! Generic functional list implementation.
//!
//! Single linked lists in Rust are tricky because there are many trade-off
//! that doesn't apply in other languages.  However, list is the natural
//! data structure for environments (which grow and shrink as we enter and
//! exit abstractions).
//!
//! The implementation below is inspired by a the famous page about too many
//! linked list, but adapted to fit the BLC compiler, notably, `next` borrows
//! the list.
//!
//! # Example usage
//! ```
//! use blc::list::*;
//!
//! // Create a list of all known allprimes
//! let allprimes = cons(7, &Nil);
//! let allprimes = cons(5, &allprimes);
//! let allprimes = cons(3, &allprimes);
//! let allprimes = cons(2, &allprimes);
//!
//! // traverse it
//! let mut primes = &allprimes;
//! while !is_empty(primes) {
//!     println!(" {}", head(primes).unwrap());
//!     primes = tail(primes).unwrap();
//! }
//!```

/// A non-empty list is a boxed list node.
#[derive(Debug, PartialEq)]
pub enum List<'a, T> {
    Nil,
    Cons { head: T, tail: &'a List<'a, T> },
}
use List::*;

/// CONStructs a list based on another.
/// Naturally `head(cons(x, -)) == Some(x)` and `tail(cons(-, l)) = Some(l)`.
pub fn cons<'a, T>(elem: T, next: &'a List<'a, T>) -> List<'a, T> {
    Cons {
        head: elem,
        tail: next,
    }
}

/// True on 0-level lists
pub fn is_empty<'a, T>(list: &'a List<'a, T>) -> bool {
    match list {
        Nil => true,
        _ => false,
    }
}

/// Returns the first element in the list wrapped in `Some` or `None` on empty lists.
pub fn head<'a, T>(list: &'a List<'a, T>) -> Option<&'a T> {
    match list {
        Nil => None,
        Cons { head, tail: _ } => Some(head),
    }
}

/// The dual to `head`, returns the head.
pub fn tail<'a, T>(list: &'a List<'a, T>) -> Option<&'a List<'a, T>> {
    match &*list {
        Nil => None,
        Cons { head: _, tail } => Some(tail),
    }
}

#[test]
fn list_works() {
    let nil: List<i32> = Nil;
    assert_eq!(is_empty(&nil), true);
    assert_eq!(head(&nil), None);
    assert_eq!(tail(&nil), None);

    let l1 = cons(1, &nil);
    assert_eq!(is_empty(&l1), false);
    assert_eq!(head(&l1), Some(&1));
    assert_eq!(tail(&l1), Some(&nil));

    let l2 = cons(2, &l1);
    assert_eq!(head(&l2), Some(&2));
    assert_eq!(tail(&l2), Some(&l1));
}

#[test]
fn example() {
    // Create a list of all known allprimes
    let allprimes = cons(7, &Nil);
    let allprimes = cons(5, &allprimes);
    let allprimes = cons(3, &allprimes);
    let allprimes = cons(2, &allprimes);

    // traverse it
    let mut primes = &allprimes;
    while !is_empty(primes) {
        println!(" {}", head(primes).unwrap());
        primes = tail(primes).unwrap();
    }
}
