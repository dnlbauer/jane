use core::cell::RefCell;
use std::rc::Rc;
pub use crate::nes::cartridge::Cartridge;
pub use crate::nes::ppu::PPU;
pub use crate::nes::cpu::CPU;
pub use crate::nes::types::*;
pub use crate::nes::bus::*;
pub use crate::nes::ppubus::*;


#[allow(non_snake_case)]
pub mod cpu;
pub mod bus;
pub mod types;
pub mod disasm;
pub mod cartridge;
pub mod mappers;
pub mod ppu;
pub mod ppubus;


// The NES class connects all elements of the NES together. It acts
// as the mediator between the different components and hold the RAM 
pub struct NES {
    pub cpu: CPU,
    pub bus: Bus,
    pub ppu: Rc<RefCell<PPU>>,
    pub ppu_bus: Rc<RefCell<PPUBus>>,
    pub clock_count: u64,
}

impl NES {
    pub fn new() -> Self {
        // The bus module needs access to the ppu to forward cpu reads to it.
        // Because the NES also requires access to the PPU for clocking, 
        // shared ownership is required. Otherwise, all
        // components except the CPU would have to live inside the bus
        // instance, which is conceptually not nice.
        let ppu = Rc::new(RefCell::new(PPU::new()));
        let ppu_bus = Rc::new(RefCell::new(PPUBus::new()));
        NES {
            cpu: CPU::new(),
            bus: Bus::new(ppu.clone(), ppu_bus.clone()),
            ppu: ppu.clone(),
            ppu_bus: ppu_bus.clone(),
            clock_count: 0,
        }
    }

    // Insert a cartridge into the NES. This inserts the cartridge bus
    // into the NES address range
    pub fn insert_cartridge(&mut self, cartridge: Cartridge) {
        // both buses need to be connected to the cartridge
        let cart = Rc::new(RefCell::new(cartridge));
        self.bus.insert_cartridge(cart.clone());
        self.ppu_bus.borrow_mut().insert_cartridge(cart.clone())
    }

    // Initializes the NES CPU programm pointer
    pub fn start(&mut self) {
        self.cpu.find_pc_addr(&self.bus);
    }

    // Reset the CPU
    pub fn reset(&mut self) {
        self.clock_count = 0;
        self.cpu.reset(&self.bus);
        self.ppu.borrow_mut().reset();
    }

    // A single clock on the NES
    pub fn clock(&mut self) {
        self.clock_count += 1;
        if self.clock_count % 3 == 0 {
            self.cpu.clock(&mut self.bus);
        }
        let mut ppu = self.ppu.borrow_mut();
        ppu.clock(&mut *self.ppu_bus.borrow_mut());
        if ppu.nmi {
            ppu.nmi = false;
            self.cpu.nmi(&mut self.bus);
            debug!("NMI triggered by PPU.")
        }
        if self.clock_count % 100000 == 0 {
            info!("clock {}", self.clock_count);
        }
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
    }

    // clock until the next scanline is done
    pub fn clock_scanline(&mut self) {
        let current_line = self.ppu.borrow().scanline;
        while self.ppu.borrow().scanline == current_line {
            self.clock();
        }
    }

    
}
