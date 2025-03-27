#![feature(macro_metavar_expr)]

pub mod bc;
pub mod vm;

#[macro_export]
macro_rules! mkindexed {
    ($t:ident => { $($n:ident),* $(,)* }) => {
        #[derive(Copy, Clone, Debug, PartialEq)]
        #[repr(usize)]
        pub enum $t {
            $( $n = ${index()} ),*
        }

        impl $t {
            #[inline]
            pub fn from_usize(x: usize) -> Self {
                match x {
                    $( ${index()} => $t::$n, )*
                    _ => unreachable!(),
                }
            }
        }
    };

    /* an indexed enum with a constant `$i` containing the length */
    (($t:ident, $i:ident) => { $($n:ident),* $(,)* }) => {
        mkindexed!($t => { $($n),* });

        pub const $i: usize = ${count($n)};
    };
}

#[macro_export]
macro_rules! mkenums {
    /* empty */
    (@e $t:ty) => { _ };

    (($x:ident, $t:ident) => { $( $n:ident $(( $($a:ty),* ))* ),* $(,)* }) => {
        #[derive(Copy, Clone, Debug, PartialEq)]
        pub enum $x {
            $( $n $(($($a),*))* ),*
        }

        $crate::mkindexed!($t => { $($n),* });

        impl $x {
            pub fn ty(&self) -> $t {
                match self {
                    $( $x::$n$(($(mkenums!(@e $a)),*))* => $t::$n ),*
                }
            }
        }
    };
}

#[macro_export]
macro_rules! err_fmt {
    ($($t:tt)*) => {{
        Err(format!($($t)*))
    }};
}

#[macro_export]
macro_rules! dbgln {
    ($($t:tt)*) => {{
        #[cfg(debug_assertions)]
        eprintln!($($t)*);
    }};
}

pub type Res<T> = Result<T, String>;
