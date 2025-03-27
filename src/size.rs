use wal::{vm::{Block, Body, VM}, bc::{Instr, InstrType, Obj}};

macro_rules! sizes {
    [$($t:ty),* $(,)*] => {{
        [$((stringify!($t), std::mem::size_of::<$t>())),*]
    }};
}

fn main() {
    sizes![Obj, Instr, InstrType, Block, Body, VM]
        .iter()
        .for_each(|(n, s)| println!("{n:10}: {}", s * 8));
}
