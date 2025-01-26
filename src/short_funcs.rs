//!  Short traits method name, e.g [`Unpack::unpack`] -> `unpack`
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
    ops::{Deref, DerefMut, Index, IndexMut},
    str::FromStr,
};

use crate::Unpack;

/// like [`Default::default`]
pub fn default<T: Default>() -> T {
    Default::default()
}

/// like [`Unpack::unpack`]
pub fn unpack<T: Unpack>(this: T) -> T::Output {
    this.unpack()
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

/// like [`Deref::deref`]
pub fn deref<T: ?Sized + Deref>(this: &T) -> &T::Target {
    this
}

/// like [`DerefMut::deref_mut`]
pub fn deref_mut<T: ?Sized + DerefMut>(this: &mut T) -> &mut T::Target {
    this
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

/// like [`|v| &v[index]`](Index::index)
///
/// # Examples
/// ```
/// # use itermaps::short_funcs::*;
/// let arr = Some(&[3, 4, 5]);
///
/// assert_eq!(arr.map(index(0)), Some(&3));
/// assert_eq!(arr.map(index(1)), Some(&4));
/// assert_eq!(arr.map(index(2)), Some(&5));
/// ```
pub fn index<T, I>(index: I) -> impl Fn(&T) -> &T::Output
where T: ?Sized + Index<I>,
      I: Clone,
{
    move |value| &value[index.clone()]
}

/// like [`|v| &mut v[index]`](IndexMut::index_mut)
pub fn index_mut<T, I>(index: I) -> impl Fn(
    &mut T
) -> &mut T::Output
where T: ?Sized + IndexMut<I>,
      I: Clone,
{
    move |value| &mut value[index.clone()]
}

/// like [`|v| v[index]`](IndexMut::index_mut)
pub fn index_owned<T, I>(index: I) -> impl Fn(
    &T
) -> T::Output
where T: ?Sized + Index<I>,
      I: Clone,
      T::Output: Copy
{
    move |value| value[index.clone()]
}
