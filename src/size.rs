use wal::bc::{Instr, InstrType, Obj};

macro_rules! sizes {
    [$($t:ty),* $(,)*] => {{
        [$((stringify!($t), std::mem::size_of::<$t>())),*]
    }};
}

fn main() {
    sizes![Obj, Instr, InstrType,]
        .iter()
        .for_each(|(n, s)| println!("{n:10}: {}", s * 8));
}
