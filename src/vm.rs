use crate::{
    Res,
    bc::{Instr, InstrType, Obj, Reg::*, Regs},
    dbgln, err_fmt, mkindexed,
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
    pub start: usize,
    /** the number of vars to allocate */
    pub vars: usize,
}

mkindexed!(BlockType => {
    Label,
    Fun,
});

/** (body index, execution time) */
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Block(pub usize, pub BlockType);

#[derive(Clone, Debug, PartialEq)]
pub struct A(Vec<Obj>);

impl A {
    #[inline]
    pub fn new() -> Self {
        Self(Vec::new())
    }

    #[inline]
    pub fn push(&mut self, x: Obj) {
        self.0.push(x);
    }
}

/** a map from `u64` to `T` */
#[derive(Clone, Debug, PartialEq)]
pub struct Map<T>(pub u64, pub HashMap<u64, T>);

impl<T> Map<T> {
    #[inline]
    pub fn new() -> Self {
        Self(0, HashMap::new())
    }

    #[inline]
    pub fn push(&mut self, x: T) -> u64 {
        let i = self.0;
        self.1.insert(i, x);
        self.0 += 1;
        i
    }
}

impl<T> Index<&u64> for Map<T> {
    type Output = T;

    #[inline]
    fn index(&self, index: &u64) -> &Self::Output {
        &self.1[index]
    }
}

impl<T> IndexMut<&u64> for Map<T> {
    #[inline]
    fn index_mut(&mut self, index: &u64) -> &mut Self::Output {
        self.1
            .get_mut(index)
            .expect(&format!("no value at key {index}"))
    }
}

/** a buffer of variables */
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

/** a stack */
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
    /** the ptr to the instruction we're currently executing */
    pub i: usize,
    /** the number of instructions */
    pub len: usize,
    pub instrs: &'a [Instr],
    pub bodies: &'a [Body],
    pub blocks: &'a [Block],
    pub flash: &'a [Obj],
    pub vecs: Map<A>,
    pub strs: Map<String>,
}

impl<'a> VM<'a> {
    pub fn new(instrs: &'a [Instr], bodies: &'a [Body], blocks: &'a [Block], flash: &'a [Obj]) -> Self {
        Self {
            i: 0,
            len: instrs.len(),
            instrs,
            bodies,
            blocks,
            flash,
            vecs: Map::new(),
            strs: Map::new(),
        }
    }

    #[inline]
    pub fn iter_body(&self, i: usize) -> BodyIter<'a> {
        BodyIter::new(self.bodies[i], self.instrs)
    }

    pub fn fmt(&self) -> String {
        format!(
            r#"INSTRS
======
{ins}
BODIES
======
{bod}
BLOCKS
======
{blk}"#,
            ins = self
                .instrs
                .iter()
                .enumerate()
                .map(|(i, x)| format!("{i:4}: {x:?}\n"))
                .collect::<String>(),
            bod = self
                .bodies
                .iter()
                .enumerate()
                .map(|(i, x)| format!(
                        "{i:4}: {x:?}\n{}",
                        self.iter_body(i)
                            .map(|x| format!("      > {x:?}\n"))
                            .collect::<String>()
                ))
                .collect::<String>(),
            blk = self
                .blocks
                .iter()
                .enumerate()
                .map(|(i, x)| format!("{i:4}: {x:?}\n"))
                .collect::<String>(),
        )
    }

    pub fn find_label(&self, n: &usize) -> Option<usize> {
        self.instrs.iter()
            .enumerate()
            .find(|(_, x)| if let Instr::Label(s) = x { n == s } else { false })
            .map(|(i, _)| i + 1)
    }

    #[inline]
    pub fn vec(&'a self, idx: &u64) -> &'a A {
        &self.vecs[idx]
    }

    #[inline]
    pub fn vec_mut(&'a mut self, idx: &u64) -> &'a mut A {
        self.vecs.1.get_mut(idx).expect(&format!("vec at {idx} not found"))
    }

    pub fn exe_instr(&mut self, r: &mut Regs, v: &mut Vars, s: &mut Stk, x: &Instr) -> Res<bool> {
        dbgln!(" > exec  {x:?}\n   where {}\n   & {:?}", r.fmt(), v);

        let t = x.ty();
        if t == InstrType::Label {
            return Ok(true);
        }

        if let Some((_, f)) = INSTRS.iter().find(|(i, _)| i == &t) {
            f(self, r, v, s, x)
        } else {
            Err(format!("invalid instruction {x:?}"))
        }
    }

    #[inline]
    pub fn inc(&mut self) {
        if self.i < self.len {
            self.i += 1;
        }
    }

    #[inline]
    pub fn exe_at(&mut self, r: &mut Regs, v: &mut Vars, s: &mut Stk, i: usize) -> Res<()> {
        dbgln!("exe_at(): starting execution at {i}");

        self.i = i;

        Ok(())
    }

    pub fn exe_body(&mut self, i: usize, cs: Option<Vec<Option<Obj>>>) -> Res<Obj> {
        dbgln!("executing body {i} with callstack {cs:?}");

        let b = self.bodies[i];
        let mut r = Regs::new();
        let mut v = if let Some(a) = cs {
            Vars(a.into_iter().rev().collect())
        } else {
            Vars::new(b.vars)
        };
        let mut s = Stk::new();

        self.i = b.start;
        while self.i < self.len {
            let x = &self.instrs[self.i];
            if x == &Instr::Ret {
                break;
            }

            let inc = self.exe_instr(&mut r, &mut v, &mut s, x)?;
            if inc {
                self.i += 1;
            }
        }

        Ok(r[RR])
    }

    #[inline]
    pub fn exe_label(&mut self, i: usize) -> Res<Obj> {
        self.exe_body(i, None)
    }

    #[inline]
    pub fn exe_fun(&mut self, i: usize, cs: Vec<Option<Obj>>) -> Res<Obj> {
        dbgln!("executing function {i} with callstack {cs:?}");
        self.exe_body(i, Some(cs))
    }

    pub fn exe_block(&mut self, i: usize) -> Res<Obj> {
        let Block(i, t) = self.blocks[i];
        match t {
            BlockType::Label => self.exe_label(i),
            BlockType::Fun => Ok(Obj::from_usize(i)),
        }
    }
}

macro_rules! un {
    ($p:pat = $x:expr => $e:expr => $r:expr) => {{
        if let $p = $x {
            $e
            Ok($r)
        } else {
            unreachable!()
        }
    }};
}

macro_rules! mkinstr {
    (math $n:ident, $op:ident, $as:ty) => {{
        mkinstr!($n(to, x)(_, reg, _, _, i) {
            let x = reg[*x];
            reg[*to].$op(x);
            dbgln!("result of {}: {:?}", stringify!($op), unsafe {
                let t: $as = transmute(reg[*to]);
                t
            });
        })
    }};

    ($t:ident($($p:pat),*)($vm:pat, $reg:pat, $var:pat, $stk:pat, $in:ident) $x:expr) => {{
        mkinstr!(ret $t($($p),*)($vm, $reg, $var, $stk, $in) {
            $x;
            Ok(true)
        })
    }};

    (ret $t:ident($($p:pat),*)($vm:pat, $reg:pat, $var:pat, $stk:pat, $in:ident) $x:expr) => {{
        (InstrType::$t, |$vm, $reg, $var, $stk, $in| {
            if let Instr::$t($($p),*) = $in {
                $x
            } else {
                unreachable!()
            }
        })
    }};
}

static INSTRS: &[(
    InstrType,
    for<'a, 'b> fn(&mut VM<'a>, &mut Regs, &mut Vars, &mut Stk, &'b Instr) -> Res<bool>,
)] = &[
    /* load things */
    mkinstr!(Lit(to, x)(_, reg, _, _, i) reg[*to] = *x),
    mkinstr!(Cpy(to, x)(_, reg, _, _, i) reg[*to] = reg[*x]),
    mkinstr!(Push(x)(_, reg, _, stk, i) stk.push(reg[*x])),
    mkinstr!(Load(to, x)(_, reg, var, _, i) if let Some(x) = var[*x] {
        reg[*to] = x;
    } else {
        return err_fmt!("var {x} not allocated")
    }),
    mkinstr!(Static(to, x)(vm, reg, _, _, i) reg[*to] = vm.flash[*x]),
    /* integer math */
    mkinstr!(DivI(x, y)(_, reg, _, _, i) {
        let y = reg[*y].as_i64();
        reg[*x].div_i(y);
    }),
    mkinstr!(math AddI, addi, i64),
    mkinstr!(math SubI, subi, i64),
    mkinstr!(math MulI, muli, i64),
    // mkinstr!(math DivI, divi, i64),
    mkinstr!(IncI(x)(_, reg, _, _, i) reg[*x].addi(Obj(1))),
    /* float math */
    mkinstr!(math AddF, addf, f64),
    mkinstr!(math SubF, subf, f64),
    mkinstr!(math MulF, mulf, f64),
    mkinstr!(math DivF, divf, f64),
    /* functions */
    mkinstr!(ret Call(ret, x)(vm, reg, _, stk, i) {
        /* function and function body */
        let f = reg[*x].as_usize();
        let fbod = vm.bodies[f];

        /* call stack */
        let cstk = (0..fbod.vars).map(|_| stk.pop()).rev().collect();

        let r = vm.exe_fun(f, cstk)?;
        dbgln!("got {r:?} from body");
        reg[*ret] = r;
        Ok(false)
    }),
    /* vector ops */
    mkinstr!(NewA(to)(vm, reg, _, _, i) {
        let i = vm.vecs.push(A::new());
        reg[*to] = Obj::from_u64(i);
    }),
    mkinstr!(PushA(a, x)(vm, reg, _, _, i) vm.vecs[&reg[*a].as_u64()].push(reg[*x])),
    /* bools */
    mkinstr!(CmpI(x, y)(_, reg, _, _, i) {
        let y = reg[*y];
        let r = reg[*x].cmpi(y);
        dbgln!("setting RCF: {}", r.as_i64());
        reg[RCF] = r;
    }),
    /* jumps */
    mkinstr!(ret Goto(lbl)(vm, reg, var, stk, i) {
        let idx = if let Some(x) = vm.find_label(lbl) {
            x
        } else {
            return err_fmt!("Goto(): label {lbl} not found");
        };
        vm.exe_at(reg, var, stk, idx)?;
        dbgln!("vm.i: {}", vm.i);
        Ok(false)
    }),
    mkinstr!(ret GotoZ(lbl)(vm, reg, var, stk, i) {
        dbgln!("RCF: {}", reg[RCF].as_i64());
        if reg[RCF].as_i64() == 0 {
            let idx = if let Some(x) = vm.find_label(lbl) {
                x
            } else {
                return err_fmt!("GotoZ(): label {lbl} not found");
            };

            vm.exe_at(reg, var, stk, idx)?;
            Ok(false)
        } else {
            Ok(true)
        }
    }),
];

#[macro_export]
macro_rules! mach {
    ($($t:expr, $v:expr => { $($x:expr),* $(,)* }),* $(,)*) => {{
        mach!([], $($t, $v => { $($x),* }),*)
    }};

    ([$($f:expr),* $(,)*], $($t:expr, $v:expr
       => { $($x:expr),+ $(,)* }
    ),* $(,)*) => {{
        use $crate::vm::{Body, Block};

        let (mut instrs, mut bodies, mut blocks, flash) = (
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::from([$($f),*]),
        );

        $(
            let i = instrs.len();
            $(instrs.push($x);)*
            let bod = $crate::push!(bodies => [Body { start: i, vars: $v }]);
            /* has to be prefixed with an underscore to suppress warning */
            let _blk = $crate::push!(blocks => [Block(bod, $t)]);
        )*

        (_blk, instrs, bodies, blocks, flash)
    }};
}

#[cfg(test)]
mod test {
    use crate::{
        bc::{Instr, Obj, Reg::*},
        dbgln, mach,
        vm::{BlockType, VM},
    };

    macro_rules! mathtest {
        ($in:ident($x:expr, $y:expr) == $r:expr) => {{
            (
                mach!(
                    [$x, $y],
                    BlockType::Label, 0 => {
                        Instr::Static(RR, 0),
                        Instr::Static(RA, 1),
                        Instr::$in(RR, RA),
                    }
                ),
                $r
            )
        }};
    }

    #[test]
    fn machines() {
        for ((i, instrs, bodies, blocks, flash), y) in [
            mathtest!(AddI(Obj::from_i64(-5), Obj::from_i64(7)) == Obj::from_i64(2)),
            mathtest!(SubI(Obj::from_i64(10), Obj::from_i64(3)) == Obj::from_i64(7)),
            mathtest!(MulI(Obj::from_i64(-8), Obj::from_i64(2)) == Obj::from_i64(-16)),
            mathtest!(DivI(Obj::from_i64(16), Obj::from_i64(2)) == Obj::from_i64(8)),
            mathtest!(AddF(Obj::from_f64(-6.), Obj::from_f64(2.)) == Obj::from_f64(-4.)),
            mathtest!(SubF(Obj::from_f64(-1.), Obj::from_f64(3.)) == Obj::from_f64(-4.)),
            mathtest!(MulF(Obj::from_f64(1.5), Obj::from_f64(10.)) == Obj::from_f64(15.)),
            mathtest!(DivF(Obj::from_f64(32.), Obj::from_f64(-2.)) == Obj::from_f64(-16.)),
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
            let mut vm = VM::new(&instrs, &bodies, &blocks, &flash);
            dbgln!("{}", vm.fmt());
            assert_eq!(vm.exe_block(i).unwrap(), y)
        }
    }
}
