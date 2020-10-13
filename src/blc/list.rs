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
//! // Create a list of all known primes
//! let allprimes = cons(2, cons(3, cons(5, cons(7, None))));
//!
//! // traverse it
//! let mul primes = allprimes;
//! while !is_empty(primes) {
//!     println!(" {}", head(primes).from_some())    
//!     primes = tail(primes);
//! }
//!```

/// A non-empty list is a boxed list node.
pub type List<'a, T> = Option<Box<ListLink<'a, T>>>;

#[derive(Debug, PartialEq)]
/// The guaranteed non-empty link in an `List`
pub struct ListLink<'a, T> {
    pub head: T,
    pub tail: &'a List<'a, T>,
}

/// CONStructs a list based on another.
/// Naturally `head(cons(x, -)) == Some(x)` and `tail(cons(-, l)) = Some(l)`.
pub fn cons<'a, T>(elem: T, next: &'a List<'a, T>) -> List<'a, T> {
    Some(Box::new(ListLink {
        head: elem,
        tail: next,
    }))
}

/// True on 0-level lists
pub fn is_empty<'a, T>(list: &'a List<'a, T>) -> bool {
    match list {
        None => true,
        Some(_) => false,
    }
}

/// Returns the first element in the list wrapped in `Some` or `None` on empty lists.
pub fn head<'a, T>(list: &'a List<'a, T>) -> Option<&'a T> {
    match list {
        None => None,
        Some(n) => Some(&n.head),
    }
}

/// The dual to `head`, returns the head.
pub fn tail<'a, T>(list: &'a List<'a, T>) -> Option<&'a List<'a, T>> {
    match &*list {
        None => None,
        Some(n) => Some(n.tail),
    }
}

#[test]
fn list_works() {
    let nil: List<i32> = None;
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
