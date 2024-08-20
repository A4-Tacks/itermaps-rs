#![cfg_attr(feature = "no_std", no_std)]
#![cfg_attr(not(feature = "no_std"), doc = include_str!("../README.md"))]

use core::{
    borrow::{Borrow, BorrowMut},
    fmt::{self, Debug},
    iter::{FusedIterator, Map},
    marker::PhantomData,
    ops::{Deref, DerefMut, Index, IndexMut},
    pin::Pin,
    str::FromStr,
};

/// Generate a closure extract fields to tuple.
///
/// ### Support exprs:
/// - `field` use value
/// - `&field` use reference
/// - `&mut field` use mutable reference
/// - `*field` use derefed value
/// - `(...)` is any tokens expr, e.g `(*&*&*&*&*&field)`
/// - `[...]` insert a rust expr, e.g `[foo(2)]`
/// - `field.sub` sub field of field
/// - `field()` call method
/// - `pat =(field)> expr` bind field into pat, return expr, e.g `n =(&1)> (n,)`
///
/// # Examples
/// **tuple fields**:
/// ```
/// # use itermaps::fields;
/// let _x: Vec<(&i32, &mut &str, char)> = [("", 'c', 2, Box::new(3))]
///     .iter_mut()
///     .map(fields!(&*3, &mut 0, 1))
///     .collect();
/// ```
///
/// **tuple and value**:
/// ```
/// # use itermaps::fields;
/// // one expr into value
/// let _x: Vec<&i32> = [(1, 2)].iter().map(fields!(&0)).collect();
/// // comma of tail, into one element tuple
/// let _y: Vec<(&i32,)> = [(1, 2)].iter().map(fields!(&0,)).collect();
/// ```
///
/// **named fields**:
/// ```
/// # use itermaps::fields;
/// struct Foo { a: i32 }
/// let _x: Vec<(i32, &mut i32)> = [Foo { a: 8 }]
///     .iter_mut()
///     .map(fields!(a, &mut a))
///     .collect();
/// ```
#[macro_export]
macro_rules! fields {
    (@extract($v:expr) &mut $($w:tt)+) => {
        &mut $crate::fields!(@extract($v) $($w)+)
    };
    (@extract($v:expr) &$($w:tt)+) => {
        &$crate::fields!(@extract($v) $($w)+)
    };
    (@extract($v:expr) *$($w:tt)+) => {
        *$crate::fields!(@extract($v) $($w)+)
    };
    (@extract($v:expr) [$($e:tt)+]) => {
        $($e)+
    };
    (@extract($v:expr) $f:tt . $g:tt . $h:tt $(($($p:tt)*))?) => {
        $v.$f.$g.$h $(($($p)*))?
    };
    (@extract($v:expr) $f:tt . $g:tt $(($($p:tt)*))?) => {
        $v.$f.$g $(($($p)*))?
    };
    (@extract($v:expr) $f:tt $(($($p:tt)*))?) => {
        $v.$f $(($($p)*))?
    };
    (@extract($v:expr) $name:pat = ($($w:tt)+) > $e:expr) => {{
        let $name = $crate::fields!(@extract($v) $($w)+);
        $e
    }};
    (@extract($v:expr) $name:pat = $w:tt > $e:expr) => {{
        let $name = $crate::fields!(@extract($v) $w);
        $e
    }};

    () => (|_| ()); // empty
    (@run() [$($e:tt)+]) => { // start one extract
        |__elem| $crate::fields!(@extract(__elem) $($e)+)
    };
    (@run() $([$($e:tt)+]),+ $(,)?) => { // start tuple
        |__elem| ($($crate::fields!(@extract(__elem) $($e)+),)+)
    };

    (@collect($($c:tt)*)) => {
        $crate::fields!(@run() $($c)*)
    };
    (@collect($($c:tt)*) ,) => {
        ::core::compile_error!(
            "unexpected comma after of input end"
        )
    };
    (@collect($($c:tt)*) , $($t:tt)+) => {
        ::core::compile_error!(
            ::core::concat!(
                "unexpected comma after of `",
                ::core::stringify!($($t)*),
                "`",
            )
        )
    };
    // rest or end, with comma
    (@collect($($c:tt)*) ($($tn:tt)+) , $($t:tt)*) => {
        $crate::fields!(@collect($($c)* [$($tn)+]) $($t)*)
    };
    (@collect($($c:tt)*) [$($tn:tt)+] , $($t:tt)*) => {
        $crate::fields!(@collect($($c)* [[$($tn)+]],) $($t)*)
    };
    (@collect($($c:tt)*) $t1:tt , $($t:tt)*) => {
        $crate::fields!(@collect($($c)* [$t1],) $($t)*)
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt , $($t:tt)*) => {
        $crate::fields!(@collect($($c)* [$t1 $t2],) $($t)*)
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt $t3:tt , $($t:tt)*) => {
        $crate::fields!(@collect($($c)* [$t1 $t2 $t3],) $($t)*)
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt $t3:tt $t4:tt , $($t:tt)*) => {
        $crate::fields!(@collect($($c)* [$t1 $t2 $t3 $t4],) $($t)*)
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt , $($t:tt)*) => {
        $crate::fields!(@collect($($c)* [$t1 $t2 $t3 $t4 $t5],) $($t)*)
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt $t6:tt , $($t:tt)*) => {
        $crate::fields!(@collect($($c)* [$t1 $t2 $t3 $t4 $t5 $t6],) $($t)*)
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt $t6:tt $t7:tt , $($t:tt)*) => {
        $crate::fields!(@collect($($c)* [$t1 $t2 $t3 $t4 $t5 $t6 $t7],) $($t)*)
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt $t6:tt $t7:tt $t8:tt , $($t:tt)*) => {
        $crate::fields!(@collect($($c)* [$t1 $t2 $t3 $t4 $t5 $t6 $t7 $t8],) $($t)*)
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt $t6:tt $t7:tt $t8:tt $t9:tt , $($t:tt)*) => {
        $crate::fields!(@collect($($c)* [$t1 $t2 $t3 $t4 $t5 $t6 $t7 $t8 $t9],) $($t)*)
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt $t6:tt $t7:tt $t8:tt $t9:tt $t10:tt , $($t:tt)*) => {
        $crate::fields!(@collect($($c)* [$t1 $t2 $t3 $t4 $t5 $t6 $t7 $t8 $t9 $t10],) $($t)*)
    };
    // end, no comma
    (@collect($($c:tt)*) ($($tn:tt)+)) => {
        $crate::fields!(@collect($($c)* [$($tn)+]))
    };
    (@collect($($c:tt)*) [$($tn:tt)+]) => {
        $crate::fields!(@collect($($c)* [[$($tn)+]]))
    };
    (@collect($($c:tt)*) $t1:tt) => {
        $crate::fields!(@collect($($c)* [$t1]))
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt) => {
        $crate::fields!(@collect($($c)* [$t1 $t2]))
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt $t3:tt) => {
        $crate::fields!(@collect($($c)* [$t1 $t2 $t3]))
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt $t3:tt $t4:tt) => {
        $crate::fields!(@collect($($c)* [$t1 $t2 $t3 $t4]))
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt) => {
        $crate::fields!(@collect($($c)* [$t1 $t2 $t3 $t4 $t5]))
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt $t6:tt) => {
        $crate::fields!(@collect($($c)* [$t1 $t2 $t3 $t4 $t5 $t6]))
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt $t6:tt $t7:tt) => {
        $crate::fields!(@collect($($c)* [$t1 $t2 $t3 $t4 $t5 $t6 $t7]))
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt $t6:tt $t7:tt $t8:tt) => {
        $crate::fields!(@collect($($c)* [$t1 $t2 $t3 $t4 $t5 $t6 $t7 $t8]))
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt $t6:tt $t7:tt $t8:tt $t9:tt) => {
        $crate::fields!(@collect($($c)* [$t1 $t2 $t3 $t4 $t5 $t6 $t7 $t8 $t9]))
    };
    (@collect($($c:tt)*) $t1:tt $t2:tt $t3:tt $t4:tt $t5:tt $t6:tt $t7:tt $t8:tt $t9:tt $t10:tt) => {
        $crate::fields!(@collect($($c)* [$t1 $t2 $t3 $t4 $t5 $t6 $t7 $t8 $t9 $t10]))
    };

    // enumerate prefix bound
    (&$($t:tt)*) => {
        $crate::fields!(@collect() &$($t)*)
    };
    (*$($t:tt)*) => {
        $crate::fields!(@collect() *$($t)*)
    };
    ($f:ident $($t:tt)*) => {
        $crate::fields!(@collect() $f $($t)*)
    };
    (($($f:tt)+) $($t:tt)*) => {
        $crate::fields!(@collect() ($($f)+) $($t)*)
    };
    ([$($f:tt)+] $($t:tt)*) => {
        $crate::fields!(@collect() [$($f)+] $($t)*)
    };
    (0 $($t:tt)*) => { $crate::fields!(@collect() 0 $($t)*) };
    (1 $($t:tt)*) => { $crate::fields!(@collect() 1 $($t)*) };
    (2 $($t:tt)*) => { $crate::fields!(@collect() 2 $($t)*) };
    (3 $($t:tt)*) => { $crate::fields!(@collect() 3 $($t)*) };
    (4 $($t:tt)*) => { $crate::fields!(@collect() 4 $($t)*) };
    (5 $($t:tt)*) => { $crate::fields!(@collect() 5 $($t)*) };
    (6 $($t:tt)*) => { $crate::fields!(@collect() 6 $($t)*) };
    (7 $($t:tt)*) => { $crate::fields!(@collect() 7 $($t)*) };
    (8 $($t:tt)*) => { $crate::fields!(@collect() 8 $($t)*) };
    (9 $($t:tt)*) => { $crate::fields!(@collect() 9 $($t)*) };
}

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

fn array_each_ref<T, const N: usize>(arr: &[T; N]) -> [&T; N] {
    use core::mem::{transmute_copy, MaybeUninit};
    let mut result: [MaybeUninit<&T>; N] = unsafe {
        MaybeUninit::uninit()
            .assume_init()
    };
    for i in 0..N {
        result[i].write(&arr[i]);
    }
    unsafe { transmute_copy(&result) }
}
fn array_each_mut<T, const N: usize>(arr: &mut [T; N]) -> [&mut T; N] {
    use core::mem::{transmute_copy, MaybeUninit};
    let mut result: [MaybeUninit<&mut T>; N] = unsafe {
        MaybeUninit::uninit()
            .assume_init()
    };
    let mut elem;
    let mut arr = &mut arr[..];
    for i in 0..N {
        (elem, arr) = arr.split_first_mut().unwrap();
        result[i].write(elem);
    }
    unsafe { transmute_copy(&result) }
}

/// Unpack, e.g `&(A, B)` into `(&A, &B)`
///
/// # Examples
/// ```
/// # use itermaps::Unpack;
/// let arr: [&i32; 2] = (&[1, 2]).unpack();
/// assert_eq!(arr, [&1, &2]);
/// ```
pub trait Unpack: Sized {
    type Output;

    /// Unpack, e.g `&(A, B)` into `(&A, &B)`
    fn unpack(self) -> Self::Output;
}
macro_rules! impls1 {
    (@impl($i:ident) $(
        $(#[$meta:meta])*
        $ty:ty $(=[$($g:tt)*]($($w:tt)*))?=> $to:ty
    ),+ $(,)? $b:block) => {
        $(
            $(#[$meta])*
            impl$(<$($g)*>)? Unpack for $ty
            $(where $($w)*)?
            {
                type Output = $to;

                fn unpack($i) -> $to $b
            }
        )+
    };
    (@impl($i:ident) $(
        $(#[$meta:meta])*
        $ty:ty $(=[$($g:tt)*])?=> $to:ty
    ),+ $(,)? $b:block) => {
        $(
            $(#[$meta])*
            impl$(<$($g)*>)? Unpack for $ty {
                type Output = $to;

                fn unpack($i) -> $to $b
            }
        )+
    };
    ($self:ident {
        $($(
            $(#[$meta:meta])*
            $ty:ty $(=[$($g:tt)*]$(($($w:tt)*))?)?=> $to:ty
        ),+ $(,)? $b:block)*
    }) => {
        $(impls1! {
            @impl($self)
            $(
                $(#[$meta])*
                $ty $(=[$($g)*]$(($($w)*))?)? => $to
            ),+ $b
        })*
    };
}
impls1!(self {
    &'_ &'a T           =['a, T: ?Sized]    => &'a T,
    &'_ mut &'a T       =['a, T: ?Sized]    => &'a T,
    &'a mut &'_ mut T   =['a, T: ?Sized]    => &'a mut T,
    {
        *self
    }

    &'a Option<T>           =['a, T]    => Option<&'a T>,
    &'a Result<T, E>        =['a, T, E] => Result<&'a T, &'a E>,
    {
        self.as_ref()
    }

    &'a mut Option<T>       =['a, T]    => Option<&'a mut T>,
    &'a mut Result<T, E>    =['a, T, E] => Result<&'a mut T, &'a mut E>,
    {
        self.as_mut()
    }

    Option<&'a mut T>       =['a, T: ?Sized] => Option<&'a T>,
    {
        self.map(|x| &*x)
    }

    Result<&'a mut T, &'a mut E>    =['a, T: ?Sized, E: ?Sized]
        => Result<&'a T, &'a E>,
    {
        match self {
            Ok(t) => Ok(t),
            Err(e) => Err(e),
        }
    }

    &'a [T; N]      =['a, T, const N: usize]    => [&'a T; N],
    {
        array_each_ref(self)
    }

    &'a mut [T; N]  =['a, T, const N: usize]    => [&'a mut T; N],
    {
        array_each_mut(self)
    }

    [&'a mut T; N]  =['a, T: ?Sized, const N: usize]    => [&'a T; N],
    {
        self.map(|x| &*x)
    }

    #[cfg(not(feature = "no_std"))]
    Box<T>  =[T]    => T,
    {
        *self
    }
});
macro_rules! impls2 {
    ($fst:ident, $st:ident $(,)?) => {
        impls2!(@impl $fst);
        impls2!(@impl #[doc = "Fake Variadic impl"] $fst, $st);
    };
    ($fst:ident $(, $i:ident)+ $(,)?) => {
        impls2!($($i),*);

        impls2!(@impl #[doc(hidden)] $fst, $($i),+);
    };
    (@impl $(#[$meta:meta])* $fst:ident $(, $i:ident)* $(,)?) => {
        $(#[$meta])*
        impl<'a, $fst, $($i),*> Unpack
        for &'a ($fst, $($i),*)
        {
            type Output = (&'a $fst, $(&'a $i),*);

            fn unpack(self) -> Self::Output {
                #[allow(non_snake_case)]
                let ($fst, $($i),*) = self;
                ($fst, $($i),*)
            }
        }
        $(#[$meta])*
        impl<'a, $fst, $($i),*> Unpack
        for &'a mut ($fst, $($i),*)
        {
            type Output = (&'a mut $fst, $(&'a mut $i),*);

            fn unpack(self) -> Self::Output {
                #[allow(non_snake_case)]
                let ($fst, $($i),*) = self;
                ($fst, $($i),*)
            }
        }
        $(#[$meta])*
        impl<'a, $fst: ?Sized, $($i: ?Sized),*> Unpack
        for (&'a mut $fst, $(&'a mut $i),*)
        {
            type Output = (&'a $fst, $(&'a $i),*);

            fn unpack(self) -> Self::Output {
                #[allow(non_snake_case)]
                let ($fst, $($i),*) = self;
                ($fst, $($i),*)
            }
        }
    };
}
impls2!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P,);
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

/// Implement commonly used combinations of [`Iterator::map`]
///
/// [`Iterator::map`]: core::iter::Iterator::map
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

    /// like `iter.map(Unpack::unpack)`
    fn map_unpack<U>(self) -> MapFn<Self, U>
    where Self::Item: Unpack<Output = U>,
    {
        self.map(Unpack::unpack)
    }

    /// like `iter.map(Into::into)`
    fn map_into<U>(self) -> MapFn<Self, U>
    where Self::Item: Into<U>,
    {
        self.map(Self::Item::into)
    }

    /// like `iter.map(TryInto::try_into)`
    fn map_try_into<U>(self) -> MapFn<Self, Result<U, <Self::Item as TryInto<U>>::Error>>
    where Self::Item: TryInto<U>,
    {
        self.map(Self::Item::try_into)
    }

    /// like `iter.map(str::parse::<U>)`
    fn map_parse<U>(self) -> MapFn<Self, Result<U, U::Err>>
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

    /// like `iter.map(Borrow::borrow::<U>)`
    fn map_borrow<'a, U>(self) -> MapFn<Self, &'a U>
    where Self::Item: ThisRef<'a>,
          U: ?Sized + 'a,
          <Self::Item as ThisRef<'a>>::Out: Borrow<U> + 'a,
    {
        self.map(|r| r.this_ref().borrow())
    }

    /// like `iter.map(BorrowMut::borrow_mut::<U>)`
    fn map_borrow_mut<'a, U>(self) -> MapFn<Self, &'a mut U>
    where Self::Item: ThisMut<'a>,
          U: ?Sized + 'a,
          <Self::Item as ThisRef<'a>>::Out: BorrowMut<U> + 'a,
    {
        self.map(|r| r.this_mut().borrow_mut())
    }

    /// like `iter.map(IntoIterator::into_iter)`
    fn map_into_iter<I>(self) -> MapFn<Self, I>
    where Self::Item: IntoIterator<IntoIter = I>,
    {
        self.map(IntoIterator::into_iter)
    }

    /// like `iter.map(FromIterator::from_iter)`
    fn map_collect<A, C>(self) -> MapFn<Self, C>
    where Self::Item: Iterator<Item = A>,
          C: FromIterator<A>,
    {
        self.map(FromIterator::from_iter)
    }

    #[cfg(not(feature = "no_std"))]
    /// like `iter.map(ToOwned::to_owned)`
    fn map_to_owned<R, U>(self) -> MapFn<Self, U>
    where Self::Item: Deref<Target = R>,
          R: ?Sized + ToOwned<Owned = U>,
    {
        self.map(|value| value.to_owned())
    }

    #[cfg(not(feature = "no_std"))]
    /// like `iter.map(ToString::to_string)`
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

/// Create from [`map_index`]
///
/// [`map_index`]: MapExt::map_index
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[cfg(not(feature = "no_std"))]
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
    }

    #[test]
    fn array_each_ref_test() {
        let mut arr = [1, 2, 3];
        let arr1 = array_each_ref(&arr);
        assert_eq!(arr1, [&1, &2, &3]);
        let arr2 = array_each_mut(&mut arr);
        assert_eq!(arr2, [&mut 1, &mut 2, &mut 3]);
        assert_eq!(arr, [1, 2, 3]);
        *array_each_mut(&mut arr)[1] += 1;
        assert_eq!(arr, [1, 3, 3]);
    }
}
