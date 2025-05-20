use core::{
    ops::Deref,
    pin::Pin,
};

/// Consume self into reference
pub trait ThisRef<'a>: 'a {
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

pub(crate) fn array_each_ref<T, const N: usize>(arr: &[T; N]) -> [&T; N] {
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
pub(crate) fn array_each_mut<T, const N: usize>(arr: &mut [T; N]) -> [&mut T; N] {
    use core::mem::{transmute_copy, MaybeUninit};
    let mut result: [MaybeUninit<&mut T>; N] = unsafe {
        MaybeUninit::uninit()
            .assume_init()
    };
    let mut elem;
    let mut arr = &mut arr[..];
    #[allow(clippy::needless_range_loop)]
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

    #[cfg(feature = "std")]
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

#[cfg(test)]
mod tests {
    use super::*;

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
