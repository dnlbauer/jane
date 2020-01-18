use crate::nes::ppu::PPU;
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
pub struct NESMemory {
    ram: [Byte; RAM_SIZE], // 2kb
    cartridge: Option<Cartridge>,
    ppu: Rc<RefCell<PPU>>,
}

impl NESMemory {
    pub fn new(ppu: Rc<RefCell<PPU>>) -> Self {
        NESMemory {
            ram: [0; RAM_SIZE],
            cartridge: None,
            ppu: ppu,
        }
    }

    pub fn insert_cartridge(&mut self, c: Cartridge) {
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

impl Memory for NESMemory {
    fn readb(&self, addr: Addr) -> Byte {
        if let Some(cartridge) = &self.cartridge {
            if CART_ADDR_RANGE[0] <= addr && addr <= CART_ADDR_RANGE[1] {
                return cartridge.readb(addr)
            } 
        }
        if RAM_ADDR_RANGE[0] <= addr && addr <= RAM_ADDR_RANGE[1] {
            // Ram is 3x mirrored after 0x07ff
            return self.ram[(addr & RAM_PHYS_RANGE[1]) as usize]
        }
        if PPU_ADDR_RANGE[0] <= addr && addr <= PPU_ADDR_RANGE[1] {
            let mut ppu = self.ppu.borrow_mut();
            return ppu.readb(addr & PPU_PHYS_RANGE[1]);
        }
        0x0000  // generic response
    }

    fn writeb(&mut self, addr: Addr, data: Byte) {
        if let Some(cartridge) = &mut self.cartridge {
            if CART_ADDR_RANGE[0] <= addr && addr <= CART_ADDR_RANGE[1] {
                cartridge.writeb(addr, data)
            } 
        }
        if RAM_ADDR_RANGE[0] <= addr && addr <= RAM_ADDR_RANGE[1] {
            // Ram is 3x mirrored after 07ff
            self.ram[(addr & RAM_PHYS_RANGE[1]) as usize] = data
        }
        if PPU_ADDR_RANGE[0] <= addr && addr <= PPU_ADDR_RANGE[1] {
            let mut ppu = self.ppu.borrow_mut();
            ppu.writeb(addr & PPU_PHYS_RANGE[1], data);
        }
    } 
}

// impl PPUMemory for NESMemory {
//     fn readb_ppu(&self, addr: Addr) -> Byte {

//     }
//     fn writeb_ppu(&mut self, addr: Addr, data: Byte) {

//     }
// }

pub trait MemoryReader {
    fn readb<T: Memory>(&self, mem: &T, addr: Addr) -> Byte {
        mem.readb(addr)
    }

    fn readw<T: Memory>(&self, mem: &T, addr: Addr) -> Word {
        mem.readw(addr)
    }

    fn writeb<T: Memory>(&mut self, mem: &mut T, addr: Addr, data: Byte) {
        mem.writeb(addr, data)
    } 
}

// PPU interface to allow read/write of memory
pub trait PPUMemory {
    fn readb_ppu(&self, addr: Addr) -> Byte;
    fn writeb_ppu(&mut self, addr: Addr, data: Byte);
}

// pub trait PPUMemoryReader {
//     fn readb_ppu<T: PPUMemory>(&self, mem: &T, addr: Addr) -> Byte {
//         mem.readb_ppu(addr)
//     }

//     fn writeb_ppu<T: PPUMemory>(&mut self, mem: &mut T, addr: Addr, data: Byte) {
//         mem.writeb_ppu(addr, data)
//     } 
// }
