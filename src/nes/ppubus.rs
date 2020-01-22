use std::rc::Rc;
use core::cell::RefCell;
use crate::nes::cartridge::Cartridge;
use crate::nes::types::*;
use crate::nes::cartridge::MirrorMode;

pub const PATTERN_MEMORY_SIZE: usize  = 4096;
pub const PATTERN_ADDR_RANGE: [Addr; 2] = [0x000, 0x1FFF];
pub const NAMETABLE_MEMORY_SIZE: usize = 1024;
pub const NAMETABLE_ADDR_RANGE: [Addr; 2] = [0x2000, 0x3EFF];
pub const PALETTE_MEMORY_SIZE: usize = 32;
pub const PALETTE_ADDR_RANGE: [Addr; 2] = [0x3F00, 0x3FFF];

pub struct PPUBus {
    pattern_memory: [[Byte; PATTERN_MEMORY_SIZE]; 2],  // 8kb pattern memory 
    nametable_memory: [[Byte; NAMETABLE_MEMORY_SIZE] ;2],  // 2kb nametables
    palette_memory: [Byte; PALETTE_MEMORY_SIZE],  // palettes
    cartridge: Option<Rc<RefCell<Cartridge>>>
}

impl PPUBus {
    pub fn new() -> Self {
        PPUBus {
            pattern_memory: [[0; PATTERN_MEMORY_SIZE]; 2], 
            nametable_memory: [[0; NAMETABLE_MEMORY_SIZE] ;2],
            palette_memory: [0; PALETTE_MEMORY_SIZE],
            cartridge: None,
        }
    }

    pub fn insert_cartridge(&mut self, c: Rc<RefCell<Cartridge>>) {
        self.cartridge = Some(c);
    }

    // map palette addr to internal memory array index
    fn map_palette_addr(&self, addr: Addr) -> usize {
        let mut rel_addr = addr - 0x3F00;
        rel_addr = rel_addr % 0x0020;
        if rel_addr == 0x0010 { rel_addr = 0x0000 }
        if rel_addr == 0x0014 { rel_addr = 0x0004 }
        if rel_addr == 0x0018 { rel_addr = 0x0008 }
        if rel_addr == 0x001C { rel_addr = 0x000C }
        rel_addr as usize
    }

    // maps a pattern table address to the index of the internal array
    fn map_pattern_table_addr(&self, addr: Addr) -> (usize, usize) {
        let table = if addr < 0x1000 { 0 } else { 1 };
        let rel_addr = addr % 0x1000;
        (table as usize, rel_addr as usize)
    }

    // maps a nametable address to the corresponding index in the memory
    // array. Method required an inserted cartrige to read the mirroring mode.
    // Returns none if no cartrige is inserted 
    fn map_nametable_addr(&self, addr: Addr) -> Option<(usize, usize)> {
        if let Some(cartridge) = &self.cartridge {
            // sovle mirroring
            let addr = addr % 0x1000;
            
            // There are four theoretical nametables
            let nametable_idx = if addr < 0x0400 {
                    0
                } else if addr < 0x0800 {
                    1
                } else if addr < 0x0C00 {
                    2
                } else {
                    3
                };

            // nametable 2,3 are actually mirrored depending on the cartriges
            // mirror mode
            let mirror_mode = cartridge.borrow().get_mirror_mode();
            let table_id = if mirror_mode == MirrorMode::VERTICAL {
                match nametable_idx {
                    0 | 2 => 0,
                    1 | 3 => 1,
                    _ => unreachable!() 
                }
            } else {
                match nametable_idx {
                    0 | 1 => 0,
                    2 | 3 => 1,
                    _ => unreachable!() 
                }
            };
            let rel_addr = addr % 0x400;
            return Some((table_id as usize, rel_addr as usize))
        }
        None
    }
}

impl PPUMemory for PPUBus {
    fn readb_ppu(&self, addr: Addr) -> Byte {
        // Palette is never mapped to cartridge
        if PALETTE_ADDR_RANGE[0] <= addr && addr <= PALETTE_ADDR_RANGE[1] {
            let idx = self.map_palette_addr(addr);
            return self.palette_memory[idx]
        }

        // give the cartridge a chance to handle the rest
        if let Some(cartridge) = &self.cartridge {
            if let Some(data) = cartridge.borrow().readb_ppu(addr) {
                return data
            }
        }

        if PATTERN_ADDR_RANGE[0] <= addr && addr <= PATTERN_ADDR_RANGE[1] {
            let idx = self.map_pattern_table_addr(addr);
            return self.pattern_memory[idx.0][idx.1]
        }
        if NAMETABLE_ADDR_RANGE[0] <= addr && addr <= NAMETABLE_ADDR_RANGE[1] {
            if let Some(idx) = &self.map_nametable_addr(addr) {
                return self.nametable_memory[idx.0][idx.1]
            }
        }
        
        0x00
    }

    fn writeb_ppu(&mut self, addr: Addr, data: Byte) {
        // Palette is never mapped to cartridge
        if PALETTE_ADDR_RANGE[0] <= addr && addr <= PALETTE_ADDR_RANGE[1] {
            let idx = self.map_palette_addr(addr);
            self.palette_memory[idx] = data;
        }

        // give the cartridge a chance to handle the rest
        if let Some(cartridge) = &self.cartridge {
            if cartridge.borrow_mut().writeb_ppu(addr, data) {
                return
            }
        }

        if PATTERN_ADDR_RANGE[0] <= addr && addr <= PATTERN_ADDR_RANGE[1] {
            let idx = self.map_pattern_table_addr(addr);
            self.pattern_memory[idx.0][idx.1] = data;
        }
        if NAMETABLE_ADDR_RANGE[0] <= addr && addr <= NAMETABLE_ADDR_RANGE[1] {
            if let Some(idx) = &self.map_nametable_addr(addr) {
                self.nametable_memory[idx.0][idx.1] = data;
            }
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

    fn dummy_ppu_bus(mirror: MirrorMode) -> PPUBus {
        let mut mem = PPUBus::new();
        let mut cart = Cartridge::dummy(mirror);
        mem.insert_cartridge(Rc::new(RefCell::new(cart)));
        mem
    }

    #[test]
    fn test_ppu_memory_nametable_rw() {
        let mut mem = dummy_ppu_bus(MirrorMode::VERTICAL);

        // read/write something to nametable memory
        for (idx, addr) in (0x2000 .. 0x2FFF + 1).enumerate() {
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
        let mut mem = dummy_ppu_bus(MirrorMode::VERTICAL);

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
                "Nametable changed unxexpected at position {:#06x}", addr);
        }  
    }

    #[test]
    fn test_ppu_memory_nametable_mirrormode_vertical() {
        let mut mem = dummy_ppu_bus(MirrorMode::VERTICAL);
        // write something to nametable memory and check if vertical mirror
        // has the same data
        
        // vertical left
        for (idx, addr) in (0x2000 .. 0x23FF + 1).enumerate() {
            mem.writeb_ppu(addr, idx as Byte);
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
            assert_eq!(idx as Byte, mem.readb_ppu(addr+0x800));
        }

        for (idx, addr) in (0x2800 .. 0x2BFF + 1).enumerate() {
            let val = (idx as Byte).wrapping_add(1);
            mem.writeb_ppu(addr, val);
            assert_eq!(val, mem.readb_ppu(addr));
            assert_eq!(val, mem.readb_ppu(addr-0x800));
        }

        // vertical right
        for (idx, addr) in (0x2400 .. 0x27FF + 1).enumerate() {
            mem.writeb_ppu(addr, idx as Byte);
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
            assert_eq!(idx as Byte, mem.readb_ppu(addr+0x800));
        }

        for (idx, addr) in (0x2C00 .. 0x2EFF + 1).enumerate() {
            let val = (idx as Byte).wrapping_add(1);
            mem.writeb_ppu(addr, val);
            assert_eq!(val, mem.readb_ppu(addr));
            assert_eq!(val, mem.readb_ppu(addr-0x800));
        }
    }

    #[test]
    fn test_ppu_memory_nametable_mirrormode_horizontal() {
        let mut mem = dummy_ppu_bus(MirrorMode::HORIZONTAL);
        // write something to nametable memory and check if horizontal mirror
        // has the same data
        
        // horizontal top
        for (idx, addr) in (0x2000 .. 0x23FF + 1).enumerate() {
            mem.writeb_ppu(addr, idx as Byte);
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
            assert_eq!(idx as Byte, mem.readb_ppu(addr+0x400));
        }

        for (idx, addr) in (0x2400 .. 0x27FF + 1).enumerate() {
            let val = (idx as Byte).wrapping_add(1);
            mem.writeb_ppu(addr, val);
            assert_eq!(val, mem.readb_ppu(addr));
            assert_eq!(val, mem.readb_ppu(addr-0x400));
        }

        // horizontal bottom
        for (idx, addr) in (0x2800 .. 0x2BFF + 1).enumerate() {
            mem.writeb_ppu(addr, idx as Byte);
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
            assert_eq!(idx as Byte, mem.readb_ppu(addr+0x400));
        }

        for (idx, addr) in (0x2C00 .. 0x2EFF + 1).enumerate() {
            let val = (idx as Byte).wrapping_add(1);
            mem.writeb_ppu(addr, val);
            assert_eq!(val, mem.readb_ppu(addr));
            assert_eq!(val, mem.readb_ppu(addr-0x400));
        }
    }

    #[test]
    fn test_ppu_memory_nametable_mirroring() {
        let mut mem = dummy_ppu_bus(MirrorMode::VERTICAL);

        // read/write something to nametable memory
        for (idx, addr) in (0x2000 .. 0x27FF + 1).enumerate() {
            mem.writeb_ppu(addr, idx as Byte);
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }

        // mirror memory should have the same data
        for (idx, addr) in (0x3000 .. 0x37FF + 1).enumerate() {
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }

        // write data to mirrored addr range
        for (idx, addr) in (0x3000 .. 0x37FF + 1).enumerate() {
            mem.writeb_ppu(addr, idx as Byte);
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }

        // start memory should have the same data
        for (idx, addr) in (0x2000 .. 0x27FF + 1).enumerate() {
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }
    }

    #[test]
    fn test_ppu_memory_palette_rw() {
        let mut mem = PPUBus::new();

        // read/write something to palette memory
        for (idx, addr) in (0x3F00 .. 0x3F1F + 1).enumerate() {
            mem.writeb_ppu(addr, idx as Byte);
            assert_eq!(idx as Byte, mem.readb_ppu(addr));
        }

        // assert palette are initialized correctly
        // mirroring makes this look a bit strange, but this is the expected
        // result for writing numbers to 0x3F00 to 0x3F1F
        let expected = [
            16,
            1, 2, 3, 20, 
            5, 6, 7, 24,
            9, 10, 11, 28, 
            13, 14, 15, 16,
            17, 18, 19, 20,
            21, 22, 23, 24,
            25, 26, 27, 28,
            29, 30, 31, 
        ];

        for (addr, expected) in (0x3F00 .. 0x3F1F + 1).zip(expected.iter()) {
            assert_eq!(expected, &mem.readb_ppu(addr),
                "{:#08x}", addr);
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
    fn test_ppu_memory_palette_internal_mirroring() {

        let wired_mirrors = [
            (0x3F10, 0x3F00),
            (0x3F14, 0x3F04),
            (0x3F18, 0x3F08),
            (0x3F1C, 0x3F0C),
        ];
        for (idx, (addr, true_addr)) in wired_mirrors.iter().enumerate() {
            let mut mem = PPUBus::new();
            mem.writeb_ppu(*true_addr as Addr, idx as Byte);
            assert_eq!(idx as Byte, mem.readb_ppu(*true_addr as Addr));
            assert_eq!(idx as Byte, mem.readb_ppu(*addr as Addr));
            mem.writeb_ppu(*true_addr as Addr, idx as Byte + 1);
            assert_eq!(idx as Byte + 1, mem.readb_ppu(*true_addr as Addr));
            assert_eq!(idx as Byte + 1, mem.readb_ppu(*addr as Addr));
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

        // assert palette are initialized correctly
        // mirroring makes this look a bit strange, but this is the expected
        // result for writing numbers to 0x3F00 to 0x3F1F
        let expected = [
            16,
            1, 2, 3, 20, 
            5, 6, 7, 24,
            9, 10, 11, 28, 
            13, 14, 15, 16,
            17, 18, 19, 20,
            21, 22, 23, 24,
            25, 26, 27, 28,
            29, 30, 31, 
        ];

        // mirror memory should have the same data
        for (idx, addr) in (0x3F20 .. 0x3F3F + 1).enumerate() {
            assert_eq!(expected[idx], mem.readb_ppu(addr));
        }
        for (idx, addr) in (0x3F40 .. 0x3F5F + 1).enumerate() {
            assert_eq!(expected[idx], mem.readb_ppu(addr));
        }
        for (idx, addr) in (0x3F60 .. 0x3F7F + 1).enumerate() {
            assert_eq!(expected[idx], mem.readb_ppu(addr));
        }
        for (idx, addr) in (0x3F80 .. 0x3F9F + 1).enumerate() {
            assert_eq!(expected[idx], mem.readb_ppu(addr));
        }
        for (idx, addr) in (0x3FA0 .. 0x3FBF + 1).enumerate() {
            assert_eq!(expected[idx], mem.readb_ppu(addr));
        }
        for (idx, addr) in (0x3FC0 .. 0x3FDF + 1).enumerate() {
            assert_eq!(expected[idx], mem.readb_ppu(addr));
        }
        for (idx, addr) in (0x3FE0 .. 0x3FFF + 1).enumerate() {
            assert_eq!(expected[idx], mem.readb_ppu(addr));
        }

        // write data to mirrored addr range
        for (idx, addr) in (0x3FC0 .. 0x3FDF + 1).enumerate() {
            mem.writeb_ppu(addr, expected[idx] + 2);
        }

        // start memory should have the same data
        for (idx, addr) in (0x3F00 .. 0x3F1F + 1).enumerate() {
            assert_eq!(expected[idx] + 2, mem.readb_ppu(addr));
        }
    }
}
