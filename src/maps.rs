use crate::{ThisMut, ThisRef, Unpack};

use core::{
    borrow::{Borrow, BorrowMut},
    fmt::{self, Debug},
    iter::{FusedIterator, Map},
    marker::PhantomData,
    ops::{Deref, DerefMut, Index, IndexMut},
    str::FromStr,
};

/// Implement commonly used combinations of [`Iterator::map`]
///
/// [`Iterator::map`]: core::iter::Iterator::map
pub trait MapExt: Iterator + Sized {
    /// like [`iter.map(Deref::deref)`](Deref::deref)
    fn map_deref<'a, R, U>(self) -> MapFn<Self, &'a U>
    where Self::Item: ThisRef<'a, Out = R>,
          R: ?Sized + Deref<Target = U> + 'a,
          U: ?Sized + 'a,
    {
        self.map(|ref_value| ref_value.this_ref().deref())
    }

    /// like [`iter.map(DerefMut::deref_mut)`](DerefMut::deref_mut)
    fn map_deref_mut<'a, R, U>(self) -> MapFn<Self, &'a mut U>
    where Self::Item: ThisMut<'a, Out = R>,
          R: ?Sized + DerefMut<Target = U> + 'a,
          U: ?Sized + 'a,
    {
        self.map(|ref_value| ref_value.this_mut().deref_mut())
    }

    /// like [`iter.map(Unpack::unpack)`](Unpack::unpack)
    fn map_unpack<U>(self) -> MapFn<Self, U>
    where Self::Item: Unpack<Output = U>,
    {
        self.map(Unpack::unpack)
    }

    /// like [`iter.map(Into::into)`](Into::into)
    fn map_into<U>(self) -> MapFn<Self, U>
    where Self::Item: Into<U>,
    {
        self.map(Self::Item::into)
    }

    /// like [`iter.map(TryInto::try_into)`](TryInto::try_into)
    fn map_try_into<U>(self) -> MapFn<Self, Result<U, <Self::Item as TryInto<U>>::Error>>
    where Self::Item: TryInto<U>,
    {
        self.map(Self::Item::try_into)
    }

    /// like [`iter.map(str::parse::<U>)`](str::parse)
    fn map_parse<U>(self) -> MapFn<Self, Result<U, U::Err>>
    where Self::Item: AsRef<str>,
          U: FromStr,
    {
        self.map(|s| s.as_ref().parse())
    }

    /// like [`iter.map(AsRef::as_ref::<U>)`](AsRef::as_ref)
    fn map_as_ref<'a, U>(self) -> MapFn<Self, &'a U>
    where Self::Item: ThisRef<'a>,
          U: ?Sized + 'a,
          <Self::Item as ThisRef<'a>>::Out: AsRef<U> + 'a,
    {
        self.map(|r| r.this_ref().as_ref())
    }

    /// like [`iter.map(AsMut::as_mut::<U>)`](AsMut::as_mut)
    fn map_as_mut<'a, U>(self) -> MapFn<Self, &'a mut U>
    where Self::Item: ThisMut<'a>,
          U: ?Sized + 'a,
          <Self::Item as ThisRef<'a>>::Out: AsMut<U> + 'a,
    {
        self.map(|r| r.this_mut().as_mut())
    }

    /// like [`iter.map(Borrow::borrow::<U>)`](Borrow::borrow)
    fn map_borrow<'a, U>(self) -> MapFn<Self, &'a U>
    where Self::Item: ThisRef<'a>,
          U: ?Sized + 'a,
          <Self::Item as ThisRef<'a>>::Out: Borrow<U> + 'a,
    {
        self.map(|r| r.this_ref().borrow())
    }

    /// like [`iter.map(BorrowMut::borrow_mut::<U>)`](BorrowMut::borrow_mut)
    fn map_borrow_mut<'a, U>(self) -> MapFn<Self, &'a mut U>
    where Self::Item: ThisMut<'a>,
          U: ?Sized + 'a,
          <Self::Item as ThisRef<'a>>::Out: BorrowMut<U> + 'a,
    {
        self.map(|r| r.this_mut().borrow_mut())
    }

    /// like [`iter.map(IntoIterator::into_iter)`](IntoIterator::into_iter)
    fn map_into_iter<I>(self) -> MapFn<Self, I>
    where Self::Item: IntoIterator<IntoIter = I>,
    {
        self.map(IntoIterator::into_iter)
    }

    /// like [`iter.map(FromIterator::from_iter)`](FromIterator::from_iter)
    fn map_collect<A, C>(self) -> MapFn<Self, C>
    where Self::Item: Iterator<Item = A>,
          C: FromIterator<A>,
    {
        self.map(FromIterator::from_iter)
    }

    #[cfg(feature = "std")]
    /// like [`iter.map(ToOwned::to_owned)`](ToOwned::to_owned)
    fn map_to_owned<R, U>(self) -> MapFn<Self, U>
    where Self::Item: Deref<Target = R>,
          R: ?Sized + ToOwned<Owned = U>,
    {
        self.map(|value| value.to_owned())
    }

    #[cfg(feature = "std")]
    /// like [`iter.map(ToString::to_string)`](ToString::to_string)
    fn map_to_string<R>(self) -> MapFn<Self, String>
    where Self::Item: Deref<Target = R>,
          R: ?Sized + ToString,
    {
        self.map(|value| value.to_string())
    }

    /// like `iter.map(|v| &v[index])`
    fn map_index<'a, R, I, U>(self, index: I) -> MapIndex<'a, Self, I>
    where Self::Item: ThisRef<'a, Out = R>,
          R: ?Sized + Index<I, Output = U>,
          U: ?Sized + 'a,
          I: Clone,
    {
        MapIndex {
            _phantom: PhantomData,
            iter: self,
            index,
        }
    }

    /// like `iter.map(|v| &mut v[index])`
    fn map_index_mut<'a, R, I, U>(self, index: I) -> MapIndexMut<'a, Self, I>
    where Self::Item: ThisMut<'a, Out = R>,
          R: ?Sized + IndexMut<I, Output = U>,
          U: ?Sized + 'a,
          I: Clone,
    {
        MapIndexMut {
            _phantom: PhantomData,
            iter: self,
            index,
        }
    }
}

impl<I: Iterator> MapExt for I {}

#[allow(type_alias_bounds)]
pub type MapFn<I: Iterator, U> = Map<I, fn(I::Item) -> U>;

macro_rules! impl_asref_asmut_self {
    (impl[$($g:tt)*] for $ty:ty $(where $($w:tt)*)?) => {
        impl<$($g)*> AsRef<Self> for $ty
        $(where $($w)*)?
        {
            fn as_ref(&self) -> &Self {
                self
            }
        }
        impl<$($g)*> AsMut<Self> for $ty
        $(where $($w)*)?
        {
            fn as_mut(&mut self) -> &mut Self {
                self
            }
        }
    };
}

/// Create from [`map_index`]
///
/// [`map_index`]: MapExt::map_index
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MapIndex<'a, I: Iterator, Idx: Clone> {
    pub(crate) _phantom: PhantomData<&'a I::Item>,
    pub(crate) iter: I,
    pub(crate) index: Idx,
}

impl<I: Iterator + Debug, Idx: Clone + Debug> Debug for MapIndex<'_, I, Idx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("MapIndex")
            .field(&self.iter)
            .field(&&self.index)
            .finish()
    }
}

impl<'a, I, T, Idx, R> Iterator for MapIndex<'a, I, Idx>
where I: Iterator,
      Idx: Clone,
      I::Item: ThisRef<'a, Out = R>,
      R: ?Sized + Index<Idx, Output = T> + 'a,
      T: ?Sized + 'a,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()?
            .this_ref()
            .index(self.index.clone())
            .into()
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.iter.nth(n)?
            .this_ref()
            .index(self.index.clone())
            .into()
    }

    fn fold<B, F>(self, init: B, mut f: F) -> B
    where Self: Sized,
          F: FnMut(B, Self::Item) -> B,
    {
        self.iter.fold(init, |acc, val| {
            let val = val.this_ref()
                .index(self.index.clone());
            f(acc, val)
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<'a, I, T, Idx, R> DoubleEndedIterator for MapIndex<'a, I, Idx>
where I: DoubleEndedIterator,
      Idx: Clone,
      I::Item: ThisRef<'a, Out = R>,
      R: ?Sized + Index<Idx, Output = T> + 'a,
      T: ?Sized + 'a,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.next_back()?
            .this_ref()
            .index(self.index.clone())
            .into()
    }

    fn rfold<B, F>(self, init: B, mut f: F) -> B
    where Self: Sized,
          F: FnMut(B, Self::Item) -> B,
    {
        self.iter.rfold(init, |acc, val| {
            let val = val.this_ref()
                .index(self.index.clone());
            f(acc, val)
        })
    }
}

impl<'a, I, T, Idx, R> ExactSizeIterator for MapIndex<'a, I, Idx>
where I: ExactSizeIterator,
      Idx: Clone,
      I::Item: ThisRef<'a, Out = R>,
      R: ?Sized + Index<Idx, Output = T> + 'a,
      T: ?Sized + 'a,
{
}

impl<'a, I, T, Idx, R> FusedIterator for MapIndex<'a, I, Idx>
where I: FusedIterator,
      Idx: Clone,
      I::Item: ThisRef<'a, Out = R>,
      R: ?Sized + Index<Idx, Output = T> + 'a,
      T: ?Sized + 'a,
{
}

/// Create from [`map_index_mut`]
///
/// [`map_index_mut`]: MapExt::map_index_mut
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MapIndexMut<'a, I: Iterator, Idx: Clone> {
    pub(crate) _phantom: PhantomData<&'a I::Item>,
    pub(crate) iter: I,
    pub(crate) index: Idx,
}

impl<I: Iterator + Debug, Idx: Clone + Debug> Debug for MapIndexMut<'_, I, Idx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("MapIndexMut")
            .field(&self.iter)
            .field(&&self.index)
            .finish()
    }
}

impl<'a, I, T, Idx, R> Iterator for MapIndexMut<'a, I, Idx>
where I: Iterator,
      Idx: Clone,
      I::Item: ThisMut<'a, Out = R>,
      R: ?Sized + IndexMut<Idx, Output = T> + 'a,
      T: ?Sized + 'a,
{
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()?
            .this_mut()
            .index_mut(self.index.clone())
            .into()
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.iter.nth(n)?
            .this_mut()
            .index_mut(self.index.clone())
            .into()
    }

    fn fold<B, F>(self, init: B, mut f: F) -> B
    where Self: Sized,
          F: FnMut(B, Self::Item) -> B,
    {
        self.iter.fold(init, |acc, val| {
            let val = val.this_mut()
                .index_mut(self.index.clone());
            f(acc, val)
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<'a, I, T, Idx, R> DoubleEndedIterator for MapIndexMut<'a, I, Idx>
where I: DoubleEndedIterator,
      Idx: Clone,
      I::Item: ThisMut<'a, Out = R>,
      R: ?Sized + IndexMut<Idx, Output = T> + 'a,
      T: ?Sized + 'a,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.next_back()?
            .this_mut()
            .index_mut(self.index.clone())
            .into()
    }

    fn rfold<B, F>(self, init: B, mut f: F) -> B
    where Self: Sized,
          F: FnMut(B, Self::Item) -> B,
    {
        self.iter.rfold(init, |acc, val| {
            let val = val.this_mut()
                .index_mut(self.index.clone());
            f(acc, val)
        })
    }
}

impl<'a, I, T, Idx, R> ExactSizeIterator for MapIndexMut<'a, I, Idx>
where I: ExactSizeIterator,
      Idx: Clone,
      I::Item: ThisMut<'a, Out = R>,
      R: ?Sized + IndexMut<Idx, Output = T> + 'a,
      T: ?Sized + 'a,
{
}

impl<'a, I, T, Idx, R> FusedIterator for MapIndexMut<'a, I, Idx>
where I: FusedIterator,
      Idx: Clone,
      I::Item: ThisMut<'a, Out = R>,
      R: ?Sized + IndexMut<Idx, Output = T> + 'a,
      T: ?Sized + 'a,
{
}

impl_asref_asmut_self! {
    impl[I, Idx] for MapIndex<'_, I, Idx>
    where I: Iterator,
          Idx: Clone,
}

impl_asref_asmut_self! {
    impl[I, Idx] for MapIndexMut<'_, I, Idx>
    where I: Iterator,
          Idx: Clone,
}

#[cfg(feature = "std")]
#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::fields;

    #[test]
    fn test_as_deref() {
        let arr = ["a".to_owned(), "b".to_owned()];
        let x: Option<&str> = arr.iter().map_deref().next();
        assert_eq!(x, Some("a"));
    }

    #[test]
    fn test_as_deref_mut() {
        let mut arr = [Box::new(2), Box::new(3)];
        let n: Option<&mut i32> = arr.iter_mut().map_deref_mut().next();
        assert_eq!(n, Some(&mut 2));
    }

    #[test]
    fn test_as_ref() {
        let arr = ["a".to_owned(), "b".to_owned()];
        let x: Option<&str> = arr.iter().map_as_ref().next();
        assert_eq!(x, Some("a"));
    }

    #[test]
    fn test_as_mut() {
        let mut arr = [Box::new(2), Box::new(3)];
        let n: Option<&mut i32> = arr.iter_mut().map_as_mut().next();
        assert_eq!(n, Some(&mut 2));
    }

    #[test]
    fn test_borrow() {
        let arr = ["a".to_owned(), "b".to_owned()];
        let x = arr.iter().map_borrow::<str>().next();
        assert_eq!(x, Some("a"));
    }

    #[test]
    fn test_borrow_mut() {
        let mut arr = [Box::new(2), Box::new(3)];
        let n: Option<&mut i32> = arr.iter_mut().map_borrow_mut().next();
        assert_eq!(n, Some(&mut 2));
    }

    #[test]
    fn test_index() {
        let mut arr = [[1, 2], [3, 4]];
        assert_eq!(arr.iter().map_index(0).copied().collect::<Vec<_>>(), [1, 3]);
        assert_eq!(arr.iter().map_index(1).copied().collect::<Vec<_>>(), [2, 4]);
        for n in arr.iter_mut().map_index_mut(1) {
            *n += 1;
        }
        assert_eq!(&arr, &[[1, 3], [3, 5]]);
    }

    #[test]
    fn test_into() {
        let arr = ["ab"];
        let x: Option<String> = arr.into_iter().map_into().next();
        assert_eq!(x, Some("ab".into()));
    }

    #[test]
    fn test_to_owned() {
        let arr = ["ab"];
        let x: Option<String> = arr.into_iter().map_to_owned().next();
        assert_eq!(x, Some("ab".into()));
    }

    #[test]
    fn test_to_string() {
        let arr = ["ab"];
        let x: Option<String> = arr.into_iter().map_to_string().next();
        assert_eq!(x, Some("ab".into()));
    }

    #[test]
    fn test_parse() {
        let arr = ["8"];
        let x: Option<Result<i32, _>> = arr.into_iter().map_parse().next();
        assert_eq!(x, Some(Ok(8)));
    }

    #[test]
    fn test_unpack() {
        let mut arr = [[1, 2, 3]];
        let unpacked = arr.iter_mut()
            .map_unpack::<[_; 3]>()
            .next();
        assert_eq!(unpacked, Some([&mut 1, &mut 2, &mut 3]));
        assert_eq!((&mut [1, 2]).unpack(), [&mut 1, &mut 2]);
    }

    #[test]
    fn test_fields() {
        let _x: Vec<i32> = [(1,)]
            .iter_mut()
            .map(fields!((*&*&*&*&*&0)))
            .collect();
        let _x: Vec<i32> = [(1,)]
            .iter_mut()
            .map(fields!(*&*&*&*&0))
            .collect();
        let _x: Vec<(&i32, &mut &str, char)> = [("", 'c', 2, Box::new(3))]
            .iter_mut()
            .map(fields!(&*3, &mut 0, 1))
            .collect();
        let _x: Vec<()> = [("", 'c', 2, Box::new(3))]
            .iter_mut()
            .map(fields!())
            .collect();
        let _x: Vec<(&str, char, i32, Box<i32>)> = [("", 'c', 2, Box::new(3))]
            .into_iter()
            .map(fields!(0, 1, 2, 3))
            .collect();
        let _x: Vec<&str> = [("", 'c', 2, Box::new(3))]
            .into_iter()
            .map(fields!((0)))
            .collect();
        let _x: Vec<(&str,)> = [("", 'c', 2, Box::new(3))]
            .into_iter()
            .map(fields!(0,))
            .collect();
        let _x: Vec<&str> = [("", 'c', 2, Box::new(3))]
            .into_iter()
            .map(fields!(0))
            .collect();

        struct Foo { a: i32 }
        struct Bar { f: Foo }
        let _x: Vec<(i32, &mut i32)> = [Foo { a: 8 }]
            .iter_mut()
            .map(fields!(a, &mut a))
            .collect();
        let _x: Vec<i32> = [Foo { a: 8 }]
            .iter_mut()
            .map(fields!(a))
            .collect();

        assert_eq!(fields!([1], [2])(0), (1, 2));
        let _x: Vec<i32> = [Bar { f: Foo { a: 8 } }]
            .iter_mut()
            .map(fields!(f.a))
            .collect();
        let _x: Vec<&mut i32> = [Bar { f: Foo { a: 8 } }]
            .iter_mut()
            .map(fields!(&mut f.a))
            .collect();
        let _x: Vec<&mut Foo> = [Bar { f: Foo { a: 8 } }]
            .iter_mut()
            .map(fields!(&mut f))
            .collect();
        let _x: Vec<(i32, Foo, i32)> = [(Foo { a: 8 }, 2)]
            .into_iter()
            .map(fields!(0.a, 0, 1))
            .collect();
        assert!(matches!(_x.into_iter().next(), Some((8, Foo { a: 8 }, 2))));
        let _x: Vec<i32> = [(0, (1, 2))]
            .into_iter()
            .map(fields!(1 .0))
            .collect();

        assert_eq!(fields!([1], [2])(0), (1, 2));
        let _x: Vec<i32> = [(0, Foo { a: 8 })]
            .into_iter()
            .map(fields!(1.a))
            .collect();

        assert_eq!(fields!([1], [2])(0), (1, 2));
        assert_eq!(fields!([1],)(0), (1,));
        assert_eq!(fields!([1])(0), 1);

        let _x: Vec<String> = [(0, Foo { a: 8 })]
            .into_iter()
            .map(fields!(1.a.to_string()))
            .collect();
        let _x: Vec<String> = [(2, 3)]
            .into_iter()
            .map(fields!(1.to_string()))
            .collect();

        let _x: Vec<(&i32,)> = [(2, 3)]
            .iter()
            .map(fields!(n =(&1)> (n,)))
            .collect();
        let _x: Vec<(i32,)> = [(2, 3)]
            .iter()
            .map(fields!(n =1> (n,)))
            .collect();

        let _x: Vec<u32> = [([0, 1i32, 2], 3u32)]
            .iter()
            .map(fields!(1))
            .collect();
        let _x: Vec<i32> = [([0, 1i32, 2], 3u32)]
            .iter()
            .map(fields!(0[1]))
            .collect();
    }
}
