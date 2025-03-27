use crate::{
    Res,
    bc::{Instr, InstrType, Obj, Reg::*, Regs},
    err_fmt, mkindexed, dbgln,
};
use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

#[macro_export]
macro_rules! push {
    ($v:expr => [ $( $x:expr ),* ]) => {{
        let i = $v.len();
        $( $v.push($x) );*;
        i
    }};
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Body {
    /** the index to start executing in `VM.instrs` */
    start: usize,
    /** the number of vars to allocate */
    vars: usize,
}

mkindexed!(BlockType => {
    Label,
    Fun,
});

/** (body index, execution time) */
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Block(pub usize, BlockType);

#[derive(Clone, Debug, PartialEq)]
pub struct A(Vec<Obj>);

/** a map from `u64` to `T` */
#[derive(Clone, Debug, PartialEq)]
pub struct Map<T>(pub HashMap<u64, T>);

impl<T> Map<T> {
    #[inline]
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}

impl<T> Index<&u64> for Map<T> {
    type Output = T;

    #[inline]
    fn index(&self, index: &u64) -> &Self::Output {
        &self.0[index]
    }
}

impl<T> IndexMut<&u64> for Map<T> {
    #[inline]
    fn index_mut(&mut self, index: &u64) -> &mut Self::Output {
        self.0
            .get_mut(index)
            .expect(&format!("no value at key {index}"))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Vars(Vec<Option<Obj>>);

impl Vars {
    pub fn new(sz: usize) -> Self {
        Self((0..sz).map(|_| None).collect())
    }
}

impl Index<usize> for Vars {
    type Output = Option<Obj>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl IndexMut<usize> for Vars {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Stk(Option<Vec<Obj>>);

impl Stk {
    pub const fn new() -> Self {
        Self(None)
    }

    pub fn push(&mut self, x: Obj) {
        if let None = self.0 {
            self.0 = Some(Vec::new());
        }

        if let Some(v) = &mut self.0 {
            v.push(x);
        }
    }

    #[inline]
    pub fn pop(&mut self) -> Option<Obj> {
        if let Some(v) = &mut self.0 {
            v.pop()
        } else {
            None
        }
    }
}

pub struct BodyIter<'a> {
    pub iter: std::slice::Iter<'a, Instr>,
}

impl<'a> BodyIter<'a> {
    pub fn new(b: Body, i: &'a [Instr]) -> Self {
        Self {
            iter: i[b.start..].iter(),
        }
    }
}

impl<'a> Iterator for BodyIter<'a> {
    type Item = &'a Instr;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .and_then(|x| if &Instr::Ret == x { None } else { Some(x) })
    }
}

pub struct VM<'a> {
    pub instrs: &'a [Instr],
    pub bodies: &'a [Body],
    pub blocks: &'a [Block],
    pub vecs: Map<A>,
    pub strs: Map<String>,
}

impl<'a> VM<'a> {
    pub fn new(instrs: &'a [Instr], bodies: &'a [Body], blocks: &'a [Block]) -> Self {
        Self {
            instrs,
            bodies,
            blocks,
            vecs: Map::new(),
            strs: Map::new(),
        }
    }

    #[inline]
    pub fn iter_body(&self, i: usize) -> BodyIter<'a> {
        BodyIter::new(self.bodies[i], self.instrs)
    }

    pub fn exe_instr(&mut self, r: &mut Regs, v: &mut Vars, s: &mut Stk, x: &'a Instr) -> Res<()> {
        dbgln!(" > exec {x:?}\n   {}", r.fmt());

        let t = x.ty();
        if let Some((_, f)) = INSTRS.iter().find(|(i, _)| i == &t) {
            f(self, r, v, s, x)
        } else {
            Err(format!("invalid instruction {x:?}"))
        }
    }

    pub fn exe_body(&mut self, i: usize, s: Option<Stk>) -> Res<Obj> {
        let b = self.bodies[i];
        let mut r = Regs::new();
        let mut v = Vars::new(b.vars);
        let mut s = if let Some(s) = s { s } else { Stk::new() };

        (0..b.vars).for_each(|i| v[i] = s.pop());

        for x in self.iter_body(i) {
            self.exe_instr(&mut r, &mut v, &mut s, x)?;
        }

        dbgln!("returning: {:?}", r[RR]);
        Ok(r[RR])
    }

    #[inline]
    pub fn exe_label(&mut self, i: usize) -> Res<Obj> {
        self.exe_body(i, None)
    }

    pub fn exe_fun(&mut self, i: usize) -> Res<Obj> {
        let stk = Stk((0..self.bodies[i].vars).map(|x| None).collect());
        self.exe_body(i, Some(stk))
    }

    pub fn exe_block(&mut self, i: usize) -> Res<Obj> {
        let Block(i, t) = self.blocks[i];
        match t {
            BlockType::Label => self.exe_label(i),
            BlockType::Fun => self.exe_fun(i),
        }
    }
}

macro_rules! un {
    ($p:pat = $x:expr => $e:expr) => {{
        if let $p = $x {
            $e
            Ok(())
        } else {
            unreachable!()
        }
    }};
}

macro_rules! mkinstr {
    (math $n:ident, $as:ident, $op:ident) => {{
        mkinstr!($n(to, x)(_, reg, _, _, i) {
            let x = reg[*x].$as();
            reg[*to].$op(x);
        })
    }};

    ($t:ident($($p:pat),*)($vm:pat, $reg:pat, $var:pat, $stk:pat, $in:ident) $x:expr) => {{
        (InstrType::$t, |$vm, $reg, $var, $stk, $in| {
            un!(Instr::$t($($p),*) = $in => { $x })
        })
    }};
}

static INSTRS: &[(
    InstrType,
    for<'a> fn(&mut VM<'a>, &mut Regs, &mut Vars, &mut Stk, &'a Instr) -> Res<()>,
)] = &[
    mkinstr!(Lit(to, x)(_, reg, _, _, i) reg[*to] = *x),
    mkinstr!(Cpy(to, x)(_, reg, _, _, i) reg[*to] = reg[*x]),
    mkinstr!(Push(x)(_, reg, _, stk, i) stk.push(reg[*x])),
    mkinstr!(Load(to, x)(_, reg, var, stk, i) if let Some(x) = var[*x] {
        reg[*to] = x;
    } else {
        return err_fmt!("var {x} not allocated")
    }),
    mkinstr!(math AddI, as_i64, add_i),
    mkinstr!(math SubI, as_i64, sub_i),
    mkinstr!(math MulI, as_i64, mul_i),
    mkinstr!(math DivI, as_i64, div_i),
    mkinstr!(math AddF, as_f64, add_f),
    mkinstr!(math SubF, as_f64, sub_f),
    mkinstr!(math MulF, as_f64, mul_f),
    mkinstr!(math DivF, as_f64, div_f),
    mkinstr!(Call(ret, x)(vm, reg, var, stk, i) {
        /* function and function body */
        let f = reg[*x].as_usize();
        let fbod = vm.bodies[f];

        /* call stack */
        let cstk = Stk((0..fbod.vars).map(|_| stk.pop()).collect());
        dbgln!("calling function {f} with callstack {cstk:?}");

        reg[*ret] = vm.exe_body(f, Some(cstk))?;
    }),
];

#[macro_export]
macro_rules! mach {
    ($($t:expr, $v:expr => { $($x:expr),* $(,)* }),* $(,)*) => {{
        let (mut instrs, mut bodies, mut blocks) = (Vec::new(), Vec::new(), Vec::new());
        $(
            let i = instrs.len();
            $(instrs.push($x);)*
            let bod = push!(bodies => [Body { start: i, vars: $v }]);
            let blk = push!(blocks => [Block(bod, $t)]);
        )*

        (blk, instrs, bodies, blocks)
    }};
}

#[cfg(test)]
mod test {
    use crate::{
        bc::{Instr, Obj, Reg::*},
        mach, push,
        vm::{Block, BlockType, Body, VM},
    };

    #[test]
    fn machines() {
        for ((i, instrs, bodies, blocks), y) in [
            (
                mach!(
                        BlockType::Label, 0 => {
                            Instr::Lit(RR, Obj::from_i64(-1)),
                            Instr::Lit(RA, Obj::from_i64(2)),
                            Instr::AddI(RR, RA),
                        }
                ),
                Obj::from_i64(1),
            ),
            (
                mach!(
                        BlockType::Label, 0 => {
                            Instr::Lit(RR, Obj::from_f64(1.)),
                            Instr::Lit(RA, Obj::from_f64(2.)),
                            Instr::AddF(RR, RA),
                        }
                ),
                Obj::from_f64(3.),
            ),
            (
                mach!(
                    /* sub */
                    BlockType::Fun, 2 => {
                        Instr::Load(RR, 0),
                        Instr::Load(RA, 1),
                        Instr::SubI(RR, RA),
                        Instr::Ret,
                    },
                    /* main */
                    BlockType::Label, 0 => {
                        Instr::Lit(RA, Obj::from_i64(10)),
                        Instr::Push(RA),

                        Instr::Lit(RA, Obj::from_i64(07)),
                        Instr::Push(RA),

                        Instr::Lit(RA, Obj::from_usize(0)),
                        Instr::Call(RR, RA),
                        Instr::Ret,
                    }
                ),
                Obj::from_i64(3),
            ),
        ]
        .into_iter()
        {
            let mut vm = VM::new(&instrs, &bodies, &blocks);
            assert_eq!(vm.exe_block(i).unwrap(), y)
        }
    }
}
