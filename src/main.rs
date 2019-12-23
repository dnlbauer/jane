#[macro_use] extern crate log;
#[macro_use] extern crate failure;
extern crate simple_logger;
extern crate phf;

#[allow(non_snake_case)]
mod cpu;
mod bus;
mod types;

use types::*;
use cpu::CPU;
use bus::{MemoryBus, Bus};

fn main() {
    simple_logger::init().unwrap();

    let mut bus = MemoryBus::new();

    // Load little test program into ram
    let test_prog: [Byte; 28] = [0xA2, 0x0A, 0x8E, 0x00, 0x00, 0xA2, 0x03, 0x8E, 0x01, 0x00, 0xAC, 0x00, 0x00, 0xA9,
    0x00, 0x18, 0x6D, 0x01, 0x00, 0x88, 0xD0, 0xFA, 0x8D, 0x02, 0x00, 0xEA, 0xEA, 0xEA];
    let offset = 0x8000;
    for i in 0..test_prog.len() {
        let addr = i + offset;
        bus.writeb(addr as u16, test_prog[i])
    }

    // hint program location to processor
    bus.writeb(0xfffc, offset as Byte);
    bus.writeb(0xfffc + 1, (offset >> 8) as Byte);

    let mut cpu: CPU = CPU::new();
    cpu.reset(&bus);
    for _ in 0..500+10 {
        cpu.clock(&mut bus);
        debug!("Mem: {} {} {}", bus.readb(0x00), bus.readb(0x01), bus.readb(0x02))
    }


}
