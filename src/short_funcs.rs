//! Short traits method name, e.g [`Unpack::unpack`] -> `unpack`
//!
//! # Examples
//! ```
//! use itermaps::short_funcs::*;
//!
//! let arr = Some((1, 2));
//! assert_eq!(arr.as_ref().map(unpack), Some((&1, &2)));
//! ```

use core::{
    borrow::{Borrow, BorrowMut},
    iter::once,
    str::FromStr,
};

pub mod ops;
pub use ops::{deref, deref_mut, index, index_mut, index_owned};
use crate::{ThisMut, ThisRef, Unpack};

/// like [`Default::default`]
pub fn default<T: Default>() -> T {
    Default::default()
}

/// like [`Unpack::unpack`]
pub fn unpack<T: Unpack>(this: T) -> T::Output {
    this.unpack()
}

/// like [`ThisRef::this_ref`]
pub fn this_ref<'a, T: ThisRef<'a>>(this: T) -> &'a T::Out {
    this.this_ref()
}

/// like [`ThisMut::this_mut`]
pub fn this_mut<'a, T: ThisMut<'a>>(this: T) -> &'a mut T::Out {
    this.this_mut()
}

/// like [`<T as Into<U>>::into`](Into::into)
pub fn into<T: Into<U>, U>(this: T) -> U {
    this.into()
}

/// like [`IntoIterator::into_iter`]
pub fn into_iter<T: IntoIterator>(this: T) -> T::IntoIter {
    this.into_iter()
}

/// like [`<T as TryInto<U>>::try_into`](TryInto::try_into)
pub fn try_into<T: TryInto<U>, U>(this: T) -> Result<U, T::Error> {
    this.try_into()
}

#[cfg(feature = "std")]
/// like [`ToOwned::to_owned`]
pub fn to_owned<T: ?Sized + ToOwned>(this: &T) -> T::Owned {
    this.to_owned()
}

/// like [`FromStr::from_str`]
pub fn parse<T: FromStr>(this: &str) -> Result<T, T::Err> {
    this.parse()
}

#[cfg(feature = "std")]
/// like [`ToString::to_string`]
pub fn to_string<T: ?Sized + ToString>(this: &T) -> String {
    this.to_string()
}

/// like [`AsRef::as_ref`]
pub fn as_ref<T: ?Sized + AsRef<U>, U>(this: &T) -> &U {
    this.as_ref()
}

/// like [`AsMut::as_mut`]
pub fn as_mut<T: ?Sized + AsMut<U>, U>(this: &mut T) -> &mut U {
    this.as_mut()
}

/// like [`Borrow::borrow`]
pub fn borrow<T: ?Sized + Borrow<U>, U>(this: &T) -> &U {
    this.borrow()
}

/// like [`BorrowMut::borrow_mut`]
pub fn borrow_mut<T: ?Sized + BorrowMut<U>, U>(this: &mut T) -> &mut U {
    this.borrow_mut()
}

/// like [`mem::copy`](core::mem::copy)
#[inline]
pub fn copy<T: Copy>(this: &T) -> T {
    *this
}

/// like [`Clone::clone`]
pub fn clone<T: Clone>(this: &T) -> T {
    this.clone()
}

/// like `|v| { f(&v); v }`
pub fn run_ref<T, F>(mut f: F) -> impl FnMut(T) -> T
where F: FnMut(&T),
{
    move |this| {
        f(&this);
        this
    }
}

/// like `|v| { f(&mut v); v }`
pub fn run_mut<T, F>(mut f: F) -> impl FnMut(T) -> T
where F: FnMut(&mut T),
{
    move |mut this| {
        f(&mut this);
        this
    }
}

/// like `|elem| collect.extend(once(elem))`
///
/// # Examples
/// ```
/// # use itermaps::short_funcs::*;
/// let mut acc = vec![0];
/// (1..3).for_each(extend_to(&mut acc));
/// assert_eq!(acc, [0, 1, 2]);
pub fn extend_to<T, C>(collect: &mut C) -> impl FnMut(T) + '_
where C: Extend<T>,
{
    |elem| collect.extend(once(elem))
}

/// like `|mut collect, elem| { collect.extend(once(elem)); collect }`
///
/// # Examples
/// ```
/// # use itermaps::short_funcs::*;
/// let acc = (1..3).fold(vec![0], extend);
/// assert_eq!(acc, [0, 1, 2]);
/// ```
pub fn extend<T, C>(mut collect: C, elem: T) -> C
where C: Extend<T>,
{
    collect.extend(once(elem));
    collect
}
