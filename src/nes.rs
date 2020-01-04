pub use crate::nes::cartridge::Cartridge;
pub use crate::nes::ppu::PPU;
pub use crate::nes::cpu::CPU;
pub use crate::nes::types::*;
pub use crate::nes::memory::*;

#[allow(non_snake_case)]
pub mod cpu;
pub mod memory;
pub mod types;
pub mod disasm;
pub mod cartridge;
pub mod mappers;
pub mod ppu;

// The NES class connects all elements of the NES together. It acts
// as the mediator between the different components and hold the RAM 
pub struct NES {
    pub cpu: CPU,
    pub ppu: PPU,
    pub memory: NESMemory,
    pub clock_count: u64,
}

impl NES {
    pub fn new() -> Self {
        NES {
            cpu: CPU::new(),
            ppu: PPU::new(),
            memory: NESMemory::new(),
            clock_count: 0,
        }
    }

    pub fn insert_cartridge(&mut self, cartridge: Cartridge) {
        self.memory.insert_cartridge(cartridge);
    }

    pub fn start(&mut self) {
        self.cpu.find_pc_addr(&self.memory);
    }

    pub fn reset(&mut self) {
        self.clock_count = 0;
        self.cpu.reset(&self.memory);
    }

    pub fn clock(&mut self) {
        self.clock_count += 1;
        if self.clock_count % 3 == 0 {
            self.cpu.clock(&mut self.memory);
        }
        self.ppu.clock(&mut self.memory);
    }
}


// impl PPUMemory for NES {
//     fn readb_ppu(&self, addr: Addr) -> Byte {
//         if let Some(cartridge) = &self.cartridge {
//             if CART_ADDR_RANGE[0] <= addr && addr <= CART_ADDR_RANGE[1] {
//                 return cartridge.readb(addr)
//             } 
//         }

//         if RAM_ADDR_RANGE[0] <= addr && addr <= RAM_ADDR_RANGE[1] {
//             // Ram is 3x mirrored after 07ff
//             return self.ram[(addr & RAM_PHYS_RANGE[1]) as usize]
//         }
//         0x0000  // generic response
//     }

//     fn writeb_ppu(&mut self, addr: Addr, data: Byte) {
//         if let Some(cartridge) = &mut self.cartridge {
//             if CART_ADDR_RANGE[0] <= addr && addr <= CART_ADDR_RANGE[1] {
//                 cartridge.writeb(addr, data)
//             } 
//         }

//         if RAM_ADDR_RANGE[0] <= addr && addr <= RAM_ADDR_RANGE[1] {
//             // Ram is 3x mirrored after 07ff
//             self.ram[(addr & RAM_PHYS_RANGE[1]) as usize] = data
//         }
//     }
// }


