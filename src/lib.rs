#![doc = include_str!("../README.md")]

use core::{
    fmt::{self, Debug},
    iter::{FusedIterator, Map},
    marker::PhantomData,
    ops::{Deref, DerefMut, Index, IndexMut},
    pin::Pin,
    str::FromStr,
};

/// Consume self into reference
pub trait ThisRef<'a> {
    type Out: ?Sized;

    /// Consume self into reference
    fn this_ref(self) -> &'a Self::Out;
}

/// Consume self into mutable reference
pub trait ThisMut<'a>: ThisRef<'a> {
    /// Consume self into mutable reference
    fn this_mut(self) -> &'a mut Self::Out;
}
impl<'a, T: ?Sized> ThisRef<'a> for &'a T {
    type Out = T;

    fn this_ref(self) -> &'a Self::Out {
        self
    }
}
impl<'a, T: ?Sized> ThisRef<'a> for &'a mut T {
    type Out = T;

    fn this_ref(self) -> &'a Self::Out {
        self
    }
}
impl<'a, T: ?Sized> ThisMut<'a> for &'a mut T {
    fn this_mut(self) -> &'a mut Self::Out {
        self
    }
}
impl<'a, P> ThisRef<'a> for Pin<P>
where P: Deref + ThisRef<'a>,
      P::Target: Unpin,
{
    type Out = P::Out;

    fn this_ref(self) -> &'a Self::Out {
        Pin::into_inner(self).this_ref()
    }
}
impl<'a, P> ThisMut<'a> for Pin<P>
where P: Deref + ThisMut<'a>,
      P::Target: Unpin,
{
    fn this_mut(self) -> &'a mut Self::Out {
        Pin::into_inner(self).this_mut()
    }
}

pub trait MapExt: Iterator + Sized {
    /// like `iter.map(Deref::deref)`
    fn map_deref<'a, R, U>(self) -> MapFn<Self, &'a U>
    where Self::Item: ThisRef<'a, Out = R>,
          R: ?Sized + Deref<Target = U> + 'a,
          U: ?Sized + 'a,
    {
        self.map(|ref_value| ref_value.this_ref().deref())
    }

    /// like `iter.map(Deref::deref_mut)`
    fn map_deref_mut<'a, R, U>(self) -> MapFn<Self, &'a mut U>
    where Self::Item: ThisMut<'a, Out = R>,
          R: ?Sized + DerefMut<Target = U> + 'a,
          U: ?Sized + 'a,
    {
        self.map(|ref_value| ref_value.this_mut().deref_mut())
    }

    /// like `iter.map(Into::into)`
    fn map_into<'a, U>(self) -> MapFn<Self, U>
    where Self::Item: Into<U>,
    {
        self.map(Self::Item::into)
    }

    /// like `iter.map(TryInto::try_into)`
    fn map_try_into<'a, U>(self) -> MapFn<Self, Result<U, <Self::Item as TryInto<U>>::Error>>
    where Self::Item: TryInto<U>,
    {
        self.map(Self::Item::try_into)
    }

    /// like `iter.map(str::parse::<U>)`
    fn map_parse<'a, U>(self) -> MapFn<Self, Result<U, U::Err>>
    where Self::Item: AsRef<str>,
          U: FromStr,
    {
        self.map(|s| s.as_ref().parse())
    }

    /// like `iter.map(AsRef::as_ref::<U>)`
    fn map_as_ref<'a, U>(self) -> MapFn<Self, &'a U>
    where Self::Item: ThisRef<'a>,
          U: ?Sized + 'a,
          <Self::Item as ThisRef<'a>>::Out: AsRef<U> + 'a,
    {
        self.map(|r| r.this_ref().as_ref())
    }

    /// like `iter.map(AsMut::as_mut::<U>)`
    fn map_as_mut<'a, U>(self) -> MapFn<Self, &'a mut U>
    where Self::Item: ThisMut<'a>,
          U: ?Sized + 'a,
          <Self::Item as ThisRef<'a>>::Out: AsMut<U> + 'a,
    {
        self.map(|r| r.this_mut().as_mut())
    }

    /// like `iter.map(ToOwned::to_owned)`
    fn map_to_owned<'a, R, U>(self) -> MapFn<Self, U>
    where Self::Item: Deref<Target = R>,
          R: ?Sized + ToOwned<Owned = U>,
    {
        self.map(|value| value.to_owned())
    }

    /// like `iter.map(ToString::to_string)`
    fn map_to_string<'a, R>(self) -> MapFn<Self, String>
    where Self::Item: Deref<Target = R>,
          R: ?Sized + ToString,
    {
        self.map(|value| value.to_string())
    }

    /// like `iter.map(|v| &v[index])`
    fn map_index<'a, R, I, U>(self, index: I) -> MapIndex<'a, Self, I>
    where Self::Item: ThisRef<'a, Out = R>,
          R: Index<I, Output = U>,
          U: 'a,
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
          R: IndexMut<I, Output = U>,
          U: 'a,
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

/// Create from [`map_index`]
///
/// [`map_index`]: MapExt::map_index
pub struct MapIndex<'a, I: Iterator, Idx: Clone> {
    _phantom: PhantomData<&'a I::Item>,
    iter: I,
    index: Idx,
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
      R: Index<Idx, Output = T> + 'a,
      T: 'a,
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
      R: Index<Idx, Output = T> + 'a,
      T: 'a,
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
      R: Index<Idx, Output = T> + 'a,
      T: 'a,
{
}
impl<'a, I, T, Idx, R> FusedIterator for MapIndex<'a, I, Idx>
where I: FusedIterator,
      Idx: Clone,
      I::Item: ThisRef<'a, Out = R>,
      R: Index<Idx, Output = T> + 'a,
      T: 'a,
{
}

/// Create from [`map_index_mut`]
///
/// [`map_index_mut`]: MapExt::map_index_mut
pub struct MapIndexMut<'a, I: Iterator, Idx: Clone> {
    _phantom: PhantomData<&'a I::Item>,
    iter: I,
    index: Idx,
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
      R: IndexMut<Idx, Output = T> + 'a,
      T: 'a,
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
      R: IndexMut<Idx, Output = T> + 'a,
      T: 'a,
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
      R: IndexMut<Idx, Output = T> + 'a,
      T: 'a,
{
}
impl<'a, I, T, Idx, R> FusedIterator for MapIndexMut<'a, I, Idx>
where I: FusedIterator,
      Idx: Clone,
      I::Item: ThisMut<'a, Out = R>,
      R: IndexMut<Idx, Output = T> + 'a,
      T: 'a,
{
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
