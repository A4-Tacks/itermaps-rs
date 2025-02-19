//! [`core::ops`] functions wrapper

use core::ops::*;
use crate::short_funcs::run_mut;

/// like [`Deref::deref`]
pub fn deref<T: ?Sized + Deref>(this: &T) -> &T::Target {
    this
}

/// like [`DerefMut::deref_mut`]
pub fn deref_mut<T: ?Sized + DerefMut>(this: &mut T) -> &mut T::Target {
    this
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

/// like [`Neg::neg`]
pub fn neg<T>(this: T) -> T::Output
where T: Neg,
{
    Neg::neg(this)
}

/// like [`Not::not`]
pub fn not<T>(this: T) -> T::Output
where T: Not,
{
    Not::not(this)
}

/// like [`Add::add`]
pub fn add<T, U>(this: T, other: U) -> T::Output
where T: Add<U>,
{
    Add::add(this, other)
}

/// like [`Sub::sub`]
pub fn sub<T, U>(this: T, other: U) -> T::Output
where T: Sub<U>,
{
    Sub::sub(this, other)
}

/// like [`Mul::mul`]
pub fn mul<T, U>(this: T, other: U) -> T::Output
where T: Mul<U>,
{
    Mul::mul(this, other)
}

/// like [`Div::div`]
pub fn div<T, U>(this: T, other: U) -> T::Output
where T: Div<U>,
{
    Div::div(this, other)
}

/// like [`Rem::rem`]
pub fn rem<T, U>(this: T, other: U) -> T::Output
where T: Rem<U>,
{
    Rem::rem(this, other)
}

/// like [`Shl::shl`]
pub fn shl<T, U>(this: T, other: U) -> T::Output
where T: Shl<U>,
{
    Shl::shl(this, other)
}

/// like [`Shr::shr`]
pub fn shr<T, U>(this: T, other: U) -> T::Output
where T: Shr<U>,
{
    Shr::shr(this, other)
}

/// like [`BitAnd::bitand`]
pub fn bitand<T, U>(this: T, other: U) -> T::Output
where T: BitAnd<U>,
{
    BitAnd::bitand(this, other)
}

/// like [`BitOr::bitor`]
pub fn bitor<T, U>(this: T, other: U) -> T::Output
where T: BitOr<U>,
{
    BitOr::bitor(this, other)
}

/// like [`BitXor::bitxor`]
pub fn bitxor<T, U>(this: T, other: U) -> T::Output
where T: BitXor<U>,
{
    BitXor::bitxor(this, other)
}

/// like [`Add::add`]
pub fn add_tuple<T, U>((this, other): (T, U)) -> T::Output
where T: Add<U>,
{
    Add::add(this, other)
}

/// like [`Sub::sub`]
pub fn sub_tuple<T, U>((this, other): (T, U)) -> T::Output
where T: Sub<U>,
{
    Sub::sub(this, other)
}

/// like [`Mul::mul`]
pub fn mul_tuple<T, U>((this, other): (T, U)) -> T::Output
where T: Mul<U>,
{
    Mul::mul(this, other)
}

/// like [`Div::div`]
pub fn div_tuple<T, U>((this, other): (T, U)) -> T::Output
where T: Div<U>,
{
    Div::div(this, other)
}

/// like [`Rem::rem`]
pub fn rem_tuple<T, U>((this, other): (T, U)) -> T::Output
where T: Rem<U>,
{
    Rem::rem(this, other)
}

/// like [`Shl::shl`]
pub fn shl_tuple<T, U>((this, other): (T, U)) -> T::Output
where T: Shl<U>,
{
    Shl::shl(this, other)
}

/// like [`Shr::shr`]
pub fn shr_tuple<T, U>((this, other): (T, U)) -> T::Output
where T: Shr<U>,
{
    Shr::shr(this, other)
}

/// like [`BitAnd::bitand`]
pub fn bitand_tuple<T, U>((this, other): (T, U)) -> T::Output
where T: BitAnd<U>,
{
    BitAnd::bitand(this, other)
}

/// like [`BitOr::bitor`]
pub fn bitor_tuple<T, U>((this, other): (T, U)) -> T::Output
where T: BitOr<U>,
{
    BitOr::bitor(this, other)
}

/// like [`BitXor::bitxor`]
pub fn bitxor_tuple<T, U>((this, other): (T, U)) -> T::Output
where T: BitXor<U>,
{
    BitXor::bitxor(this, other)
}

/// like `run_mut(|this| AddAssign::add_by(this, value.clone()))`
pub fn add_by<T, U>(value: U) -> impl FnMut(T) -> T
where T: AddAssign<U>,
      U: Clone,
{
    run_mut(move |this| AddAssign::add_assign(this, value.clone()))
}

/// like `run_mut(|this| SubAssign::sub_by(this, value.clone()))`
pub fn sub_by<T, U>(value: U) -> impl FnMut(T) -> T
where T: SubAssign<U>,
      U: Clone,
{
    run_mut(move |this| SubAssign::sub_assign(this, value.clone()))
}

/// like `run_mut(|this| MulAssign::mul_by(this, value.clone()))`
pub fn mul_by<T, U>(value: U) -> impl FnMut(T) -> T
where T: MulAssign<U>,
      U: Clone,
{
    run_mut(move |this| MulAssign::mul_assign(this, value.clone()))
}

/// like `run_mut(|this| DivAssign::div_by(this, value.clone()))`
pub fn div_by<T, U>(value: U) -> impl FnMut(T) -> T
where T: DivAssign<U>,
      U: Clone,
{
    run_mut(move |this| DivAssign::div_assign(this, value.clone()))
}

/// like `run_mut(|this| RemAssign::rem_by(this, value.clone()))`
pub fn rem_by<T, U>(value: U) -> impl FnMut(T) -> T
where T: RemAssign<U>,
      U: Clone,
{
    run_mut(move |this| RemAssign::rem_assign(this, value.clone()))
}

/// like `run_mut(|this| ShlAssign::shl_by(this, value.clone()))`
pub fn shl_by<T, U>(value: U) -> impl FnMut(T) -> T
where T: ShlAssign<U>,
      U: Clone,
{
    run_mut(move |this| ShlAssign::shl_assign(this, value.clone()))
}

/// like `run_mut(|this| ShrAssign::shr_by(this, value.clone()))`
pub fn shr_by<T, U>(value: U) -> impl FnMut(T) -> T
where T: ShrAssign<U>,
      U: Clone,
{
    run_mut(move |this| ShrAssign::shr_assign(this, value.clone()))
}

/// like `run_mut(|this| BitandAssign::bitand_by(this, value.clone()))`
pub fn bitand_by<T, U>(value: U) -> impl FnMut(T) -> T
where T: BitAndAssign<U>,
      U: Clone,
{
    run_mut(move |this| BitAndAssign::bitand_assign(this, value.clone()))
}

/// like `run_mut(|this| BitorAssign::bitor_by(this, value.clone()))`
pub fn bitor_by<T, U>(value: U) -> impl FnMut(T) -> T
where T: BitOrAssign<U>,
      U: Clone,
{
    run_mut(move |this| BitOrAssign::bitor_assign(this, value.clone()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ops_by_test() {
        let mut iter = [1, 2, 3]
            .into_iter()
            .map(add_by(1));
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(4));
    }

    #[test]
    fn ops_test() {
        let mut iter = [(1, 2), (2, 3)]
            .into_iter()
            .map(add_tuple);
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(5));
    }
}
