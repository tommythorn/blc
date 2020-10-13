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

impl<'a, T> List<'a, T> {
    /// CONStructs a list based on another.
    /// Naturally `head(cons(x, -)) == Some(x)` and `tail(cons(-, l)) = Some(l)`.
    pub fn cons(self: &'a List<'a, T>, elem: T) -> List<'a, T> {
        Cons {
            head: elem,
            tail: self,
        }
    }

    /// True on 0-level lists
    pub fn is_empty(self: &'a List<'a, T>) -> bool {
        matches!(self, Nil)
    }

    /// Returns the first element in the list wrapped in `Some` or `None` on empty lists.
    pub fn head(self: &'a List<'a, T>) -> &'a T {
        match self {
            Nil => panic!("head on empty list"),
            Cons { head, tail: _ } => head,
        }
    }

    /// The dual to `head`, returns the head.
    pub fn tail(self: &'a List<'a, T>) -> &'a List<'a, T> {
        match &*self {
            Nil => panic!("tail on empty list"),
            Cons { head: _, tail } => tail,
        }
    }
}

#[test]
fn list_works() {
    let nil: &List<i32> = &Nil;
    assert_eq!(nil.is_empty(), true);

    let l1 = nil.cons(1);
    assert_eq!(l1.is_empty(), false);
    assert_eq!(l1.head(), &1);
    assert_eq!(l1.tail(), nil);

    let l2 = l1.cons(2);
    assert_eq!(l2.head(), &2);
    assert_eq!(l2.tail(), &l1);
}

#[test]
fn example() {
    // Create a list of all known allprimes
    let allprimes = Nil.cons(7);
    let allprimes = allprimes.cons(5);
    let allprimes = allprimes.cons(3);
    let allprimes = allprimes.cons(2);

    // traverse it
    let mut primes = &allprimes;
    while !primes.is_empty() {
        println!(" {}", primes.head());
        primes = primes.tail();
    }
}
