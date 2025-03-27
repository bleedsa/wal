use crate::{mkenums, mkindexed};
use std::{
    mem::transmute,
    ops::{Index, IndexMut},
};

macro_rules! impl_math {
    ($n:ident($x:ident: $xt:ty, $y:ident: $yt:ty) => $e:expr) => {
        #[inline]
        pub fn $n(&mut self, $y: $yt) {
            unsafe {
                let $x: $xt = transmute(self.0);
                self.0 = transmute($e);
            }
        }
    };
}

/**
 * a bytecode object.
 *
 * can only store 64 bits of data. literally transmute atoms into a u64 using
 * `from_*()` methods. heap allocated objects are stored in buffers in the vm
 * structure and referenced by id (usize).
 */
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Obj(pub u64);

impl Obj {
    #[inline]
    pub fn from_usize(x: usize) -> Self {
        unsafe { Self(transmute(x)) }
    }

    #[inline]
    pub fn from_f64(x: f64) -> Self {
        unsafe { Self(transmute(x)) }
    }

    #[inline]
    pub fn as_f64(&self) -> f64 {
        unsafe { transmute(self.0) }
    }

    #[inline]
    pub fn from_i64(x: i64) -> Self {
        unsafe { Self(transmute(x)) }
    }

    #[inline]
    pub fn as_i64(&self) -> i64 {
        unsafe { transmute(self.0) }
    }

    impl_math!(add_i(x: i64, y: i64) => x+y);
    impl_math!(sub_i(x: i64, y: i64) => x-y);
    impl_math!(mul_i(x: i64, y: i64) => x*y);
    impl_math!(div_i(x: i64, y: i64) => x/y);

    impl_math!(add_f(x: f64, y: f64) => x+y);
    impl_math!(sub_f(x: f64, y: f64) => x-y);
    impl_math!(mul_f(x: f64, y: f64) => x*y);
    impl_math!(div_f(x: f64, y: f64) => x/y);
}

mkindexed!((Reg, REGN) => {
    /* the musical notes :)
     * main work registers */
    RA, RB, RC, RD, RE, RF, RG,
    /* return */
    RR,
});

pub struct Regs(pub [Obj; REGN]);

impl Regs {
    pub const fn new() -> Self {
        Self([Obj(0); REGN])
    }
}

impl Index<Reg> for Regs {
    type Output = Obj;

    #[inline]
    fn index(&self, index: Reg) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl IndexMut<Reg> for Regs {
    #[inline]
    fn index_mut(&mut self, index: Reg) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

mkenums!((Instr, InstrType) => {
    /* push reg to stack */
    Push(Reg),
    /* pop from stack and move into reg */
    Pop(Reg),

    /* copy y into x */
    Cpy(Reg, Reg),
    /* copy literal object y into x */
    Lit(Reg, Obj),

    /* math */
    AddI(Reg, Reg), SubI(Reg, Reg), MulI(Reg, Reg), DivI(Reg, Reg),
    AddF(Reg, Reg), SubF(Reg, Reg), MulF(Reg, Reg), DivF(Reg, Reg),

    /* return from the current body */
    Ret(Reg),
});
