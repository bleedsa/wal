use crate::{
    dbgln, mkenums, mkindexed,
    vm::{A, VM},
};
use std::{
    arch::asm,
    mem::transmute,
    ops::{Index, IndexMut},
};

macro_rules! impl_math {
    ($n:ident($x:ident: $xt:ty, $y:ident: $yt:ty) => $e:expr) => {
        #[inline]
        pub fn $n(&mut self, $y: $yt) {
            unsafe {
                let $x: $xt = transmute(self.0);
                dbgln!("{}: {}", stringify!($e), $e);
                self.0 = transmute($e);
            }
        }
    };
}

macro_rules! impl_asm_math {
    ($n:ident($x:ident, $y:ident) => { $($t:tt)* }) => {
        impl_asm_math!($n($x: i64, $y: i64) => { $($t)* });
    };

    ($n:ident($x:ident: $xt:ty, $y:ident: $yt:ty) => { $($t:tt)* }) => {
        #[inline]
        pub fn $n(&mut self, y: Obj) {
            let (mut $x, $y): ($xt, $yt) = unsafe {
                (transmute(self.0), transmute(y.0))
            };

            /* TODO: why is this dbgln load bearing? */
//            dbgln!("{}()\tx: {}, y: {}", stringify!($n), $x, $y);

            unsafe {
                asm!($($t)*);
                self.0 = transmute($x);
            };
        }
    };
}

macro_rules! obj_into {
    [$($t:ty => [$to:ident, $from:ident]),* $(,)*] => {
        $(
            #[inline]
            pub fn $to(&self) -> $t {
                unsafe {
                    transmute(self.0)
                }
            }

            #[inline]
            pub fn $from(x: $t) -> Self {
                unsafe {
                    Self(transmute(x))
                }
            }
        )*
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
    obj_into![
        u64 => [as_u64, from_u64],
        usize => [as_usize, from_usize],
        f64 => [as_f64, from_f64],
        i64 => [as_i64, from_i64],
    ];

    #[inline]
    pub fn as_a<'a>(&self, vm: &'a VM<'a>) -> &'a A {
        vm.vec(&self.as_u64())
    }

    /*
    pub fn cmpi(&mut self, y: Obj) -> i64 {
        let (x, y): (i64, i64) = unsafe {
            (transmute(self.0), transmute(y.0))
        };
        let cf: i64;
        let zf: i64;

        unsafe {
            asm!(
                "cmp {x}, {y}",
                "mov zf, {zf}",
                "mov {cf}, cf",
                x = in(reg) x,
                y = in(reg) y,
                cf = out(reg) cf,
                zf = out(reg) zf,
            );
            dbgln!("cmpi()\tx: {x}, y: {y} = ({zf}, {cf})");
        };


        unsafe { cf }
    }
    */

    impl_asm_math!(addi(x, y) => {
        "add {x}, {y}",
        x = inout(reg) x,
        y = in(reg) y,
    });
    impl_asm_math!(subi(x, y) => {
        "sub {x}, {y}",
        x = inout(reg) x,
        y = in(reg) y,
    });
    impl_asm_math!(muli(x, y) => {
        "imul {x}, {y}",
        x = inout(reg) x,
        y = in(reg) y,
    });
    /*
    impl_asm_math!(divi(x, y) => {
        "idiv {x}, {y}",
        x = inout(reg) x,
        y = in(reg) y,
    });
    */

    impl_math!(add_i(x: i64, y: i64) => x+y);
    impl_math!(sub_i(x: i64, y: i64) => x-y);
    impl_math!(mul_i(x: i64, y: i64) => x*y);
    impl_math!(div_i(x: i64, y: i64) => x/y);

    impl_asm_math!(addf(x: f64, y: f64) => {
        "addsd {x}, {y}",
        x = inout(xmm_reg) x,
        y = in(xmm_reg) y,
    });
    impl_asm_math!(subf(x: f64, y: f64) => {
        "subsd {x}, {y}",
        x = inout(xmm_reg) x,
        y = in(xmm_reg) y,
    });
    impl_asm_math!(mulf(x: f64, y: f64) => {
        "mulsd {x}, {y}",
        x = inout(xmm_reg) x,
        y = in(xmm_reg) y,
    });
    impl_asm_math!(divf(x: f64, y: f64) => {
        "divsd {x}, {y}",
        x = inout(xmm_reg) x,
        y = in(xmm_reg) y,
    });

    impl_math!(add_f(x: f64, y: f64) => x+y);
    impl_math!(sub_f(x: f64, y: f64) => x-y);
    impl_math!(mul_f(x: f64, y: f64) => x*y);
    impl_math!(div_f(x: f64, y: f64) => x/y);

    pub fn cmpi(&self, y: Obj) -> Obj {
        let r: i64;
        let (x, y): (i64, i64) = unsafe { (transmute(self.0), transmute(y.0)) };

        unsafe {
            asm!(
                "mov {r}, {x}",
                "sub {r}, {y}",
                r = out(reg) r,
                x = in(reg) x,
                y = in(reg) y,
            );
        }

        Obj::from_i64(r)
    }

    #[inline]
    pub fn cmp_i(&self, x: Obj) -> Obj {
        Obj::from_i64(self.as_i64() - x.as_i64())
    }
}

mkindexed!((Reg, REGN) => {
    /* the musical notes :)
     * main work registers */
    RA, RB, RC, RD, RE, RF, RG,
    /* return */
    RR,
    /* comparison flag */
    RCF,
});

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Regs(pub [Obj; REGN]);

impl Regs {
    pub const fn new() -> Self {
        Self([Obj(0); REGN])
    }

    #[inline]
    pub fn fmt(&self) -> String {
        self.0
            .iter()
            .enumerate()
            .map(|(i, o)| format!("{:4?} = {o:?}", Reg::from_usize(i)))
            .collect::<Vec<_>>()
            .join(", ")
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
    /* load var y into reg x */
    Load(Reg, usize),
    /* store reg y into var x */
    Store(usize, Reg),
    /* load static object y into reg x */
    Static(Reg, usize),
    /* a label */
    Label(usize),

    /* copy y into x */
    Cpy(Reg, Reg),
    /* copy literal object y into x */
    Lit(Reg, Obj),

    /* math */
    IncI(Reg), DecI(Reg), IncF(Reg), DecF(Reg),
    AddI(Reg, Reg), SubI(Reg, Reg), MulI(Reg, Reg), DivI(Reg, Reg),
    AddF(Reg, Reg), SubF(Reg, Reg), MulF(Reg, Reg), DivF(Reg, Reg),

    /* bools and jumps */
    CmpI(Reg, Reg),
    Goto(usize),
    GotoZ(usize),
    GotoNZ(usize),

    /* call a function y and place return value in x */
    Call(Reg, Reg),

    /* vector ops */
    NewA(Reg),
    PushA(Reg, Reg),
    PopA(Reg, Reg),

    /* return from the current body */
    Ret,
});
