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
    };

    /* an indexed enum with a constant `$i` containing the length */
    (($t:ident, $i:ident) => { $($n:ident),* $(,)* }) => {
        #[derive(Copy, Clone, Debug, PartialEq)]
        #[repr(usize)]
        pub enum $t {
            $( $n = ${index()} ),*
        }

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

pub type Res<T> = Result<T, String>;
