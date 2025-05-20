#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(feature = "std", doc = include_str!("../README.md"))]

mod maps;
mod utils;
mod filter;
pub mod short_funcs;

pub use maps::*;
pub use utils::*;
pub use filter::*;

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
/// - `field[...]` index value
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
    (@extract($v:expr) $f:tt . $g:tt . $h:tt $([$($p:tt)*])?) => {
        $v.$f.$g.$h $([$($p)*])?
    };
    (@extract($v:expr) $f:tt . $g:tt $([$($p:tt)*])?) => {
        $v.$f.$g $([$($p)*])?
    };
    (@extract($v:expr) $f:tt $([$($p:tt)*])?) => {
        $v.$f $([$($p)*])?
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
