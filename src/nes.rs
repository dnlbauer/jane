use crate::nes::bus::*;
use crate::nes::cartridge::Cartridge;
use crate::nes::ppu::*;
use crate::nes::cpu::*;

#[allow(non_snake_case)]
pub mod cpu;
pub mod bus;
pub mod types;
pub mod disasm;
pub mod cartridge;
pub mod mappers;
pub mod ppu;


pub struct NES {
    pub bus: Bus,
    pub cpu: CPU,
    pub ppu: PPU,
    pub clock_count: u64,
}

impl NES {
    pub fn new() -> Self {
        NES {
            bus: Bus::new(),
            cpu: CPU::new(),
            ppu: PPU::new(),
            clock_count: 0,
        }
    }

    pub fn insert_cartrige(&mut self, cartrige: Cartridge) {
        self.bus.insert_cartrige(cartrige);
    }

    pub fn clock(&mut self) {
        self.clock_count += 1;
        if self.clock_count % 3 == 0 {
            self.cpu.clock(&mut self.bus);
        }
        self.ppu.clock(&mut self.bus);

    }
}

