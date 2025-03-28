use wal::{mach, bc::{Reg::*, Instr, Obj}, vm::{Body, Block, BlockType, VM}};

type Mach = (usize, Vec<Instr>, Vec<Body>, Vec<Block>, Vec<Obj>);

fn iota() -> Mach {
    mach!(
        [
            /* !n */
            Obj::from_i64(10000),
            /* fun 0 = iota */
            Obj::from_usize(0)
        ],
        BlockType::Fun, 1 => {
            Instr::Lit(RC, Obj::from_i64(0)), /* counter */
            Instr::Load(RB, 0),               /* limit */
            Instr::NewA(RR),
            Instr::Label(0),
            Instr::CmpI(RC, RB),
            Instr::GotoZ(1),
            Instr::PushA(RR, RC),
            Instr::IncI(RC),
            Instr::Goto(0),
            Instr::Label(1),
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
        |o: &Obj, vm: &VM| format!("{}", o.$m())
    }};

    (vm $m:ident) => {{
        |o: &Obj, vm: &VM| format!("{:?}", o.$m(vm))
    }};
}

fn main() {
    let m: &[(fn(&Obj, &VM) -> String, Mach)] = &[
        (fmt_obj!(as_i64), mach!([Obj::from_i64(32), Obj::from_i64(-2)],
            BlockType::Label, 0 => {
                Instr::Static(RR, 0),
                Instr::Static(RA, 1),
                Instr::DivI(RR, RA),
            }
        )),
        (fmt_obj!(vm as_a), iota()),
    ];

    for (f, (i, instrs, bodies, blocks, flash)) in m.into_iter() {
        let mut vm = VM::new(&instrs, &bodies, &blocks, &flash);
        println!("{}", vm.fmt());
        println!("{}", vm.exe_block(*i).map(|x| f(&x, &vm)).unwrap_or_else(|e| format!("err: {e}")));
    }
}
