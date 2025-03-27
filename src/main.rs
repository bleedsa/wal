use wal::{mach, bc::{Reg::*, Instr, Obj}, vm::{BlockType, VM}};

fn main() {
    for (i, instrs, bodies, blocks, flash) in [
        mach!([Obj::from_i64(32), Obj::from_i64(-2)],
            BlockType::Label, 0 => {
                Instr::Static(RR, 0),
                Instr::Static(RA, 1),
                Instr::DivI(RR, RA),
            }
        )
    ].into_iter()
    {
        let mut vm = VM::new(&instrs, &bodies, &blocks, &flash);
        println!("{}", vm.fmt());
        println!("{:?}", vm.exe_block(i));
    }
}
