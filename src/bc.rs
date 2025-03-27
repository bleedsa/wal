use crate::{mkenums, mkindexed};
use std::{
    mem::transmute,
    ops::{Index, IndexMut},
};

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
    pub fn from_usize(x: usize) -> Self {
        unsafe { Self(transmute(x)) }
    }

    pub fn from_f64(x: f64) -> Self {
        unsafe { Self(transmute(x)) }
    }

    pub fn as_f64(&self) -> f64 {
        unsafe { transmute(self.0) }
    }

    pub fn add_i(&mut self, x: u64) {
        self.0 += x;
    }

    pub fn add_f(&mut self, x: f64) {
        unsafe {
            let f: f64 = transmute(self.0);
            self.0 = transmute(f + x);
        }
    }
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
    AddI(Reg, Reg),
    AddF(Reg, Reg),

    /* return from the current body */
    Ret(Reg),
});
