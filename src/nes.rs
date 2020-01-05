use core::cell::RefCell;
use std::rc::Rc;
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
    pub ppu: Rc<RefCell<PPU>>,
    pub memory: NESMemory,
    pub clock_count: u64,
}

impl NES {
    pub fn new() -> Self {
        // The memory module needs access to some parts of the nes like the ppu
        // to forward cpu reads to it. Because the NES also requires access
        // to the PPU for clocking, we use shared ownership. Otherwise, all
        // components except the CPU would have to live inside the memory
        // instance, which is conceptually not what we are looking for.
        let ppu = Rc::new(RefCell::new(PPU::new()));
        NES {
            cpu: CPU::new(),
            ppu: ppu.clone(),
            memory: NESMemory::new(ppu.clone()),
            clock_count: 0,
        }
    }

    // Insert a cartridge into the NES. This inserts the cartridge memory
    // into the NES address range
    pub fn insert_cartridge(&mut self, cartridge: Cartridge) {
        self.memory.insert_cartridge(cartridge);
    }

    // Initializes the NES CPU programm pointer
    pub fn start(&mut self) {
        self.cpu.find_pc_addr(&self.memory);
    }

    // Reset the CPU
    pub fn reset(&mut self) {
        self.clock_count = 0;
        self.cpu.reset(&self.memory);
    }

    // A single clock on the NES
    pub fn clock(&mut self) {
        self.clock_count += 1;
        if self.clock_count % 3 == 0 {
            self.cpu.clock(&mut self.memory);
        }
        self.ppu.borrow_mut().clock(&mut self.memory);
    }


    // clock until the next cpu instruction is run
    pub fn clock_instruction(&mut self) {
        if !self.cpu.is_ahead() {
            while !self.cpu.is_ahead() {
                self.clock();
            }
        } 
        while self.cpu.is_ahead() {
            self.clock();
        }
    }

    // clock until the next frame is ready
    pub fn clock_frame(&mut self) {
        while !self.ppu.borrow().frame_ready {
            self.clock();
        }
        self.ppu.borrow_mut().frame_ready = false;
        info!("clock {}", self.clock_count);
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


