use core::iter::Filter;

macro_rules! ty {
    () => { Filter<Self, impl FnMut(&Self::Item) -> bool> };
}

/// Implement commonly used combinations of [`Iterator::filter`]
///
/// [`Iterator::filter`]: core::iter::Iterator::filter
pub trait FilterExt: Iterator + Sized {
    /// like `.filter(|&x| predicate(x))`
    fn filter_copied<F>(self, mut predicate: F) -> ty!()
    where Self::Item: Copy,
          F: FnMut(Self::Item) -> bool,
    {
        self.filter(move |&elem| predicate(elem))
    }

    /// like `.filter(|elem| elem != value)`
    fn filter_ne<T>(self, value: &T) -> ty!()
    where Self::Item: PartialEq<T>,
    {
        self.filter(move |elem| elem != value)
    }

    /// like `.filter(|iter| iter.any(f))`
    fn filter_any<F>(self, mut f: F) -> ty!()
    where for<'a> &'a Self::Item: IntoIterator,
          F: FnMut(<&Self::Item as IntoIterator>::Item) -> bool,
    {
        self.filter(move |inner| inner.into_iter().any(&mut f))
    }

    /// like `.filter(|iter| iter.all(f))`
    fn filter_all<F>(self, mut f: F) -> ty!()
    where for<'a> &'a Self::Item: IntoIterator,
          F: FnMut(<&Self::Item as IntoIterator>::Item) -> bool,
    {
        self.filter(move |inner| inner.into_iter().all(&mut f))
    }

    /// like `.filter(|elem| ! predicate(elem))`
    fn exclude<F>(self, mut predicate: F) -> ty!()
    where F: FnMut(&Self::Item) -> bool,
    {
        self.filter(move |elem| ! predicate(elem))
    }

    /// like [`.exclude(|iter| iter.any(f))`](#method.exclude)
    fn exclude_any<F>(self, mut f: F) -> ty!()
    where for<'a> &'a Self::Item: IntoIterator,
          F: FnMut(<&Self::Item as IntoIterator>::Item) -> bool,
    {
        self.exclude(move |inner| inner.into_iter().any(&mut f))
    }

    /// like [`.exclude(|iter| iter.all(f))`](#method.exclude)
    fn exclude_all<F>(self, mut f: F) -> ty!()
    where for<'a> &'a Self::Item: IntoIterator,
          F: FnMut(<&Self::Item as IntoIterator>::Item) -> bool,
    {
        self.exclude(move |inner| inner.into_iter().all(&mut f))
    }
}
impl<I: Iterator> FilterExt for I {}

#[test]
fn it_works() {
    let mut iter = [0, 1, 2, 3].iter().filter_ne(&&2);
    assert_eq!(iter.next(), Some(&0));
    assert_eq!(iter.next(), Some(&1));
    assert_eq!(iter.next(), Some(&3));
    assert_eq!(iter.next(), None);
}
