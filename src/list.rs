//! Generic functional list implementation.
//!
//! Single linked lists in Rust are tricky because there are many trade-off
//! that doesn't apply in other languages.  However, list is the natural
//! data structure for environments (which grow and shrink as we enter and
//! exit abstractions).
//!
//! The implementation below is inspired by a the famous page about too many
//! linked list, but very different.  Note that the tail is *borrowed*, which
//! happens to make the implementation simpler.
//!

/// A singly linked list with a borrowed tail
///
/// # Example usage
/// ```
/// use blc::list::*;
/// use List::*;
/// // Create a list of all known primes
/// let allprimes = Nil.cons(7);
/// let allprimes = allprimes.cons(5);
/// let allprimes = allprimes.cons(3);
/// let allprimes = allprimes.cons(2);
///
/// // traverse it
/// let mut primes = &allprimes;
/// let mut sum_of_all_primes = 0;
/// while !primes.is_empty() {
///     println!(" {}", primes.head());
///     sum_of_all_primes += primes.head();
///     primes = primes.tail();
/// }
/// assert_eq!(sum_of_all_primes, 17);
/// ```
#[derive(Debug, PartialEq)]
pub enum List<'a, T> {
    Nil,
    Cons { head: T, tail: &'a List<'a, T> },
}
use List::*;

impl<'a, T> List<'a, T> {
    /// Borrows a tails and constructs a list with one more element.
    pub fn cons(self: &'a List<'a, T>, elem: T) -> List<'a, T> {
        Cons {
            head: elem,
            tail: self,
        }
    }

    /// True on 0-length lists.
    pub fn is_empty(self: &'a List<'a, T>) -> bool {
        matches!(self, Nil)
    }

    /// Returns the first element in the list.
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
