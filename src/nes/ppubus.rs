use crate::nes::ppu::PPU;
use std::rc::Rc;
use core::cell::RefCell;
use crate::nes::cartridge::Cartridge;
use crate::nes::types::*;

pub const PATTERN_MEMORY_SIZE: usize  = 4096;
pub const PATTERN_ADDR_RANGE: [Addr; 2] = [0x000, 0x1FFF];
pub const NAMETABLE_MEMORY_SIZE: usize = 1024;
pub const NAMETABLE_ADDR_RANGE: [Addr; 2] = [0x2000, 0x3EFF];
pub const PALETTE_MEMORY_SIZE: usize = 32;
pub const PALETTE_ADDR_RANGE: [Addr; 2] = [0x3F00, 0x3FFF];

pub struct PPUBus {
    pattern_memory: [[Byte; PATTERN_MEMORY_SIZE]; 2],  // 8kb pattern memory 
    nametable_memory: [[Byte; NAMETABLE_MEMORY_SIZE] ;4],  // 2kb nametables
    palette_memory: [Byte; PALETTE_MEMORY_SIZE],  // palettes
    cartridge: Option<Rc<RefCell<Cartridge>>>
}

impl PPUBus {
    pub fn new() -> Self {
        PPUBus {
            pattern_memory: [[0; PATTERN_MEMORY_SIZE]; 2], 
            nametable_memory: [[0; NAMETABLE_MEMORY_SIZE] ;4],
            palette_memory: [0; PALETTE_MEMORY_SIZE],
            cartridge: None,
        }
    }

    pub fn insert_cartridge(&mut self, c: Rc<RefCell<Cartridge>>) {
        self.cartridge = Some(c);
    }
}

impl PPUMemory for PPUBus {
    fn readb_ppu(&self, addr: Addr) -> Byte {
        // Palette is never mapped to cartridge
        if PALETTE_ADDR_RANGE[0] <= addr && addr <= PALETTE_ADDR_RANGE[1] {
            let rel_addr = addr - 0x3F00;
            return self.palette_memory[(rel_addr % 0x0020) as usize]
        }

        // give the cartridge a chance to handle the rest
        if let Some(cartridge) = &self.cartridge {
            if let Some(data) = cartridge.borrow().readb_ppu(addr) {
                return data
            }
        }

        if PATTERN_ADDR_RANGE[0] <= addr && addr <= PATTERN_ADDR_RANGE[1] {
            let table = if addr < 0x1000 { 0 } else { 1 };
            return self.pattern_memory[table as usize][(addr % 0x1000) as usize]
        }
        if NAMETABLE_ADDR_RANGE[0] <= addr && addr <= NAMETABLE_ADDR_RANGE[1] {
            let table = if addr < 0x2400 {
                0
            } else if addr < 0x2800 {
                1
            } else if addr < 0x2C00 {
                2
            } else {
                3
            };
            let rel_addr = addr - 0x2000;
            return self.nametable_memory[table][(rel_addr % 0x400) as usize]
        }
        
        0x00
    }
    fn writeb_ppu(&mut self, addr: Addr, data: Byte) {
        // Palette is never mapped to cartridge
        if PALETTE_ADDR_RANGE[0] <= addr && addr <= PALETTE_ADDR_RANGE[1] {
            let rel_addr = addr - 0x3F00;
            self.palette_memory[(rel_addr % 0x0020) as usize] = data;
        }

        // give the cartridge a chance to handle the rest
        if let Some(cartridge) = &self.cartridge {
            if cartridge.borrow_mut().writeb_ppu(addr, data) {
                return
            }
        }

        if PATTERN_ADDR_RANGE[0] <= addr && addr <= PATTERN_ADDR_RANGE[1] {
            let table = if addr < 0x1000 { 0 } else { 1 };
            self.pattern_memory[table as usize][(addr % 0x1000) as usize] = data;
        }
        if NAMETABLE_ADDR_RANGE[0] <= addr && addr <= NAMETABLE_ADDR_RANGE[1] {
            let table = if addr < 0x2400 {
                0
            } else if addr < 0x2800 {
                1
            } else if addr < 0x2C00 {
                2
            } else {
                3
            };
            let rel_addr = addr - 0x2000;
            self.nametable_memory[table][(rel_addr % 0x400) as usize] = data;
        }
    }
}

// PPU interface to allow read/write of memory
pub trait PPUMemory {
    fn readb_ppu(&self, addr: Addr) -> Byte;
    fn writeb_ppu(&mut self, addr: Addr, data: Byte);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ppu_memory_pattern() {
        let mut mem = PPUBus::new();
        
        // table/position 0,0
        assert_eq!(mem.readb_ppu(0x0), 0);
        mem.writeb_ppu(0x0, 100);
        assert_eq!(mem.readb_ppu(0x0), 100);

        // table/position 0,1
        assert_eq!(mem.readb_ppu(0x1), 0);
        mem.writeb_ppu(0x1, 101);
        assert_eq!(mem.readb_ppu(0x1), 101);
        
        // table/position 0,1000
        assert_eq!(mem.readb_ppu(0x0FFF), 0);
        mem.writeb_ppu(0x0FFF, 102);
        assert_eq!(mem.readb_ppu(0x0FFF), 102);

        // table/position 1,0
        assert_eq!(mem.readb_ppu(0x1000), 0);
        mem.writeb_ppu(0x1000, 103);
        assert_eq!(mem.readb_ppu(0x1000), 103);

        // table/position 1,1
        assert_eq!(mem.readb_ppu(0x1001), 0);
        mem.writeb_ppu(0x1001, 104);
        assert_eq!(mem.readb_ppu(0x1001), 104);
        
        // table/position 1,1000
        assert_eq!(mem.readb_ppu(0x1FFF), 0);
        mem.writeb_ppu(0x1FFF, 105);
        assert_eq!(mem.readb_ppu(0x1FFF), 105);

        // not pattern
        assert_eq!(mem.readb_ppu(0x2000), 0);
    }

    #[test]
    fn test_ppu_memory_pattern_no_overwrite() {
        let mut mem = PPUBus::new();
        
        // fill pattern
        for addr in 0x0 .. 0x1FFF + 1 {
            mem.writeb_ppu(addr, 0x1);
        } 

        // fill remaining addr space
        for addr in 0x2000 .. 0x3EFF + 1 {
            mem.writeb_ppu(addr, 0x2);
        }
        for addr in 0x3F00 .. 0x3FFF + 1 {
            mem.writeb_ppu(addr, 0x3);
        }

        // pattern should not have changed
        for addr in 0x0 .. 0x1FFF + 1 {
            assert_eq!(0x1, mem.readb_ppu(addr));
        }        
    }

    #[test]
    fn test_ppu_memory_nametable_rw() {
        let mut mem = PPUBus::new();

        // read/write something to nametable memory
        for (idx, addr) in (0x2000 .. 0x2FFF + 1).enumerate() {
            assert_eq!(0, mem.readb_ppu(addr));
            mem.writeb_ppu(addr, idx as Byte);
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }

        // assert nametables are initialized correctly
        for (idx, addr) in (0x2000 .. 0x2FFF + 1).enumerate() {
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }
    }

    #[test]
    fn test_ppu_memory_nametable_not_overwritten() {
        let mut mem = PPUBus::new();

        // write some value to whole nametable space        
        for addr in 0x2000 .. 0x3EFF + 1 {
            mem.writeb_ppu(addr, 0x1);
        }
        // write something else to the remaining address space
        for addr in 0x0 .. 0x1FFF + 1 {
            mem.writeb_ppu(addr, 0x2);
        }
        for addr in 0x3F00 .. 0x3FFF + 1 {
            mem.writeb_ppu(addr, 0x3);
        }

        // Namestables should not have changed
        for addr in 0x2000 .. 0x3EFF + 1 {
            assert_eq!(0x1, mem.readb_ppu(addr),
                "Nametable changed unxexpected at position {:#08x}", addr);
        }  
    }

    #[test]
    fn test_ppu_memory_nametable_mirroring() {
        let mut mem = PPUBus::new();

        // read/write something to nametable memory
        for (idx, addr) in (0x2000 .. 0x2FFF + 1).enumerate() {
            mem.writeb_ppu(addr, idx as Byte);
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }

        // mirror memory should have the same data
        for (idx, addr) in (0x3000 .. 0x3EFF + 1).enumerate() {
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }

        // write data to mirrored addr range
        for (idx, addr) in (0x3000 .. 0x3EFF + 1).enumerate() {
            mem.writeb_ppu(addr, idx as Byte);
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }

        // start memory should have the same data
        for (idx, addr) in (0x2000 .. 0x2FFF + 1).enumerate() {
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }
    }

    #[test]
    fn test_ppu_memory_palette_rw() {
        let mut mem = PPUBus::new();

        // read/write something to palette memory
        for (idx, addr) in (0x3F00 .. 0x3F1F + 1).enumerate() {
            assert_eq!(0, mem.readb_ppu(addr));
            mem.writeb_ppu(addr, idx as Byte);
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }

        // assert palette are initialized correctly
        for (idx, addr) in (0x3F00 .. 0x3F1F + 1).enumerate() {
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }
    }

    #[test]
    fn test_ppu_memory_palette_not_overwritten() {
        let mut mem = PPUBus::new();

        // write some value to whole palette space        
        for addr in 0x3F00 .. 0x3F1F + 1 {
            mem.writeb_ppu(addr, 0x1);
        }
        // write something else to the remaining address space
        for addr in 0x0 .. 0x1FFF + 1 {
            mem.writeb_ppu(addr, 0x2);
        }
        for addr in 0x2000 .. 0x3EFF + 1 {
            mem.writeb_ppu(addr, 0x3);
        }

        // palette should not have changed
        for addr in 0x3F00 .. 0x3F1F + 1 {
            assert_eq!(0x1, mem.readb_ppu(addr),
                "Palette changed unxexpected at position {:#08x}", addr);
        }  
    }

    #[test]
    fn test_ppu_memory_palette_mirroring() {
        let mut mem = PPUBus::new();

        // read/write something to palette memory
        for (idx, addr) in (0x3F00 .. 0x3F1F + 1).enumerate() {
            mem.writeb_ppu(addr, idx as Byte);
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }

        // mirror memory should have the same data
        for (idx, addr) in (0x3F20 .. 0x3F3F + 1).enumerate() {
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }
        for (idx, addr) in (0x3F40 .. 0x3F5F + 1).enumerate() {
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }
        for (idx, addr) in (0x3F60 .. 0x3F7F + 1).enumerate() {
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }
        for (idx, addr) in (0x3F80 .. 0x3F9F + 1).enumerate() {
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }
        for (idx, addr) in (0x3FA0 .. 0x3FBF + 1).enumerate() {
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }
        for (idx, addr) in (0x3FC0 .. 0x3FDF + 1).enumerate() {
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }
        for (idx, addr) in (0x3FE0 .. 0x3FFF + 1).enumerate() {
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }

        // write data to mirrored addr range
        for (idx, addr) in (0x3FC0 .. 0x3FDF + 1).enumerate() {
            mem.writeb_ppu(addr, idx as Byte + 1);
        }

        // start memory should have the same data
        for (idx, addr) in (0x3F00 .. 0x3F1F + 1).enumerate() {
            assert_eq!(idx as Byte + 1, mem.readb_ppu(addr));
        }
    }
}
