use wal::{mach, bc::{Reg::*, Instr, Obj}, vm::{Body, Block, BlockType, VM}};

type Mach = (usize, Vec<Instr>, Vec<Body>, Vec<Block>, Vec<Obj>);

fn iota() -> Mach {
    mach!(
        [
            /* !3 */
            Obj::from_i64(3),
            /* fun 0 = iota */
            Obj::from_usize(0)
        ],
        BlockType::Fun, 1 => {
            Instr::Lit(RC, Obj::from_i64(0)), /* counter */
            Instr::Load(RB, 0),               /* limit */
            Instr::NewA(RR),
            Instr::Label("loop"),
            Instr::CmpI(RC, RB),
            Instr::GotoZ("end"),
            Instr::PushA(RR, RC),
            Instr::IncI(RC),
            Instr::Goto("loop"),
            Instr::Label("end"),
            Instr::Ret,
        },
        BlockType::Label, 0 => {
            Instr::Static(RA, 0),
            Instr::Push(RA),
            Instr::Static(RA, 1),
            Instr::Call(RR, RA),
            Instr::Ret,
        },
    )
}

macro_rules! fmt_obj {
    ($m:ident) => {{
        |o: &Obj| format!("{}", o.$m())
    }};
}

fn main() {
    let m: &[(fn(&Obj) -> String, Mach)] = &[
        (fmt_obj!(as_i64), mach!([Obj::from_i64(32), Obj::from_i64(-2)],
            BlockType::Label, 0 => {
                Instr::Static(RR, 0),
                Instr::Static(RA, 1),
                Instr::DivI(RR, RA),
            }
        )),
        (fmt_obj!(as_u64), iota()),
    ];

    for (f, (i, instrs, bodies, blocks, flash)) in m.into_iter() {
        let mut vm = VM::new(&instrs, &bodies, &blocks, &flash);
        println!("{}", vm.fmt());
        println!("{:?}", vm.exe_block(*i).map(|x| f(&x)));
    }
}
