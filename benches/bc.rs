use criterion::{Criterion, criterion_group, criterion_main};
use wal::{mach, vm::{VM, Block, Body, BlockType}, bc::{Instr, Obj, Reg::*}};

type Mach = (usize, Vec<Instr>, Vec<Body>, Vec<Block>, Vec<Obj>);

fn iota(n: i64) -> Mach {
        mach!(
        [
            /* !n */
            Obj::from_i64(n),
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

fn bench(c: &mut Criterion) {
    for (n, x) in [1_000_000, 10_000_000]
        .into_iter()
        .enumerate()
    {
        let (i, instrs, bodies, blocks, flash) = iota(x);
        let mut vm = VM::new(&instrs, &bodies, &blocks, &flash);
        c.bench_function(&format!("{n:3}: iota({x:12})"), |ctx| {
            ctx.iter(|| vm.exe_block(i).unwrap())
        });
    }
}

criterion_group!(benches, bench);
criterion_main!(benches);
