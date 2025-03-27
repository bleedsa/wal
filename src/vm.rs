use crate::{
    Res,
    bc::{Instr, InstrType, Obj, Reg::*, Regs},
    mkindexed,
};
use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
    mem::transmute,
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
pub struct Vars(Option<Vec<Option<Obj>>>);

impl Vars {
    pub fn new(sz: usize) -> Self {
        Self(if sz == 0 {
            None
        } else {
            (0..sz).map(|_| None).collect()
        })
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
            .and_then(|x| if let Instr::Ret(_) = x { None } else { Some(x) })
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

    pub fn exe_instr(&mut self, r: &mut Regs, v: &mut Vars, x: &'a Instr) -> Res<()> {
        let t = x.ty();
        if let Some((_, f)) = INSTRS.iter().find(|(i, _)| i == &t) {
            f(self, r, v, x)
        } else {
            Err(format!("invalid instruction {x:?}"))
        }
    }

    pub fn exe_body(&mut self, i: usize) -> Res<Obj> {
        let b = self.bodies[i];
        let mut r = Regs::new();
        let mut v = Vars::new(b.vars);

        for x in self.iter_body(i) {
            self.exe_instr(&mut r, &mut v, x)?;
        }

        Ok(r[RR])
    }

    pub fn exe_block(&mut self, i: usize) -> Res<Obj> {
        let Block(i, t) = self.blocks[i];
        self.exe_body(i)
    }
}

macro_rules! un {
    ($p:pat = $x:expr => { $($ex:stmt);*; }) => {{
        if let $p = $x {
            $($ex);*;
            Ok(())
        } else {
            unreachable!()
        }
    }};
}

static INSTRS: &[(InstrType, for<'a> fn(&mut VM<'a>, &mut Regs, &mut Vars, &'a Instr) -> Res<()>)] = &[
    (InstrType::Lit, |_, reg, _, i| {
        if let Instr::Lit(to, x) = i {
            reg[*to] = *x;
            Ok(())
        } else {
            unreachable!()
        }
    }),
    (InstrType::AddI, |_, reg, _, i| un!(Instr::AddI(to, x) = i => {
        let int = reg[*x].0;
        reg[*to].add_i(int);
    })),
    (InstrType::AddF, |_, reg, _, i| un!(Instr::AddF(to, x) = i => {
        let flt = reg[*x].as_f64();
        reg[*to].add_f(flt);
    })),
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
    use crate::{push, mach, vm::{VM, BlockType, Block, Body}, bc::{Obj, Instr, Reg::*}};

    #[test]
    fn machines() {
        for ((i, instrs, bodies, blocks), y) in [
            (mach!(
                    BlockType::Label, 0 => {
                        Instr::Lit(RR, Obj(1)),
                        Instr::Lit(RA, Obj(2)),
                        Instr::AddI(RR, RA),
                    }
            ), Obj(3)),
            (mach!(
                    BlockType::Label, 0 => {
                        Instr::Lit(RR, Obj::from_f64(1.)),
                        Instr::Lit(RA, Obj::from_f64(2.)),
                        Instr::AddF(RR, RA),
                    }
            ), Obj::from_f64(3.)),
        ].into_iter() {
            let mut vm = VM::new(&instrs, &bodies, &blocks);
            assert_eq!(vm.exe_block(i).unwrap(), y)
        }
    }
}
