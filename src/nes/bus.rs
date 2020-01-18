use crate::nes::ppu::PPU;
use crate::nes::ppubus::PPUBus;
use std::rc::Rc;
use core::cell::RefCell;
use crate::nes::cartridge::Cartridge;
use crate::nes::types::*;

pub const RAM_SIZE: usize  = 0x0800;
pub const RAM_ADDR_RANGE: [Addr; 2] = [0x0000, 0x1fff];
pub const RAM_PHYS_RANGE: [Addr; 2] = [0x0000, 0x07ff];
pub const PPU_ADDR_RANGE: [Addr; 2] = [0x2000, 0x3fff];
pub const PPU_PHYS_RANGE: [Addr; 2] = [0x2000, 0x2007];
pub const CART_ADDR_RANGE: [Addr; 2] = [0x4020, 0xffff];

// NES memory: Contains data from RAM, cartridge...
pub struct Bus {
    ram: [Byte; RAM_SIZE], // 2kb
    cartridge: Option<Rc<RefCell<Cartridge>>>,
    ppu: Rc<RefCell<PPU>>,
    ppu_bus: Rc<RefCell<PPUBus>>,
}

impl Bus {
    pub fn new(ppu: Rc<RefCell<PPU>>, ppu_bus: Rc<RefCell<PPUBus>>) -> Self {
        Bus {
            ram: [0; RAM_SIZE],
            cartridge: None,
            ppu: ppu,
            ppu_bus: ppu_bus,
        }
    }

    pub fn insert_cartridge(&mut self, c: Rc<RefCell<Cartridge>>) {
        self.cartridge = Some(c);
    }
}

pub trait Memory {
    fn readb(&self, addr: Addr) -> Byte;
    fn writeb(&mut self, addr: Addr, data: Byte);
    fn readw(&self, addr: Addr) -> Word {
        let lo = self.readb(addr);
        let hi = self.readb(addr+1);
        (hi as Word) << 8 | lo as Word
    }
    fn writew(&mut self, addr: Addr, data: Word) {
        self.writeb(addr, data as Byte);
        self.writeb(addr + 1, (data >> 8) as Byte);
    }
}

impl Memory for Bus {
    fn readb(&self, addr: Addr) -> Byte {
        if let Some(cartridge) = &self.cartridge {
            if CART_ADDR_RANGE[0] <= addr && addr <= CART_ADDR_RANGE[1] {
                return cartridge.borrow().readb(addr).unwrap()  // does not fail.
            } 
        }
        if RAM_ADDR_RANGE[0] <= addr && addr <= RAM_ADDR_RANGE[1] {
            // Ram is 3x mirrored after 0x07ff
            return self.ram[(addr & RAM_PHYS_RANGE[1]) as usize]
        }
        if PPU_ADDR_RANGE[0] <= addr && addr <= PPU_ADDR_RANGE[1] {
            let mut ppu = self.ppu.borrow_mut();
            let ppu_bus = self.ppu_bus.borrow();
            return ppu.readb(&*ppu_bus, addr & PPU_PHYS_RANGE[1]);
        }
        0x0000  // generic response
    }

    fn writeb(&mut self, addr: Addr, data: Byte) {
        if let Some(cartridge) = &mut self.cartridge {
            if CART_ADDR_RANGE[0] <= addr && addr <= CART_ADDR_RANGE[1] {
                cartridge.borrow_mut().writeb(addr, data);
            } 
        }
        if RAM_ADDR_RANGE[0] <= addr && addr <= RAM_ADDR_RANGE[1] {
            // Ram is 3x mirrored after 07ff
            self.ram[(addr & RAM_PHYS_RANGE[1]) as usize] = data
        }
        if PPU_ADDR_RANGE[0] <= addr && addr <= PPU_ADDR_RANGE[1] {
            let mut ppu = self.ppu.borrow_mut();
            let mut ppu_bus = self.ppu_bus.borrow_mut();
            ppu.writeb(&mut *ppu_bus, addr & PPU_PHYS_RANGE[1], data);
        }
    } 
}