use wal::{
    bc::{Instr, InstrType, Obj},
    vm::{Block, BlockType, Body, VM},
};

macro_rules! sizes {
    [$($t:ty),* $(,)*] => {{
        [$((stringify!($t), std::mem::size_of::<$t>())),*]
    }};
}

fn main() {
    sizes![
        Obj, Option<Obj>,
        Instr, InstrType,
        Body,
        Block, BlockType,
        VM
    ]
        .iter()
        .for_each(|(n, s)| println!("{n:12}: {}", s * 8));
}
