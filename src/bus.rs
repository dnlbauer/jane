use crate::types::*;

pub trait Bus {
    fn readb(&self, addr: Addr) -> Byte;
    fn readw(&self, addr: Addr) -> Word;
    fn writeb(&mut self, addr: Addr, data: Byte);
}

pub struct MemoryBus {
    ram: [Byte; 0xFFFF]
}

impl MemoryBus {
    pub fn new() -> MemoryBus {
        MemoryBus {
            ram: [0; 0xFFFF]
        }
    }
}

impl Bus for MemoryBus {
    
    fn readb(&self, addr: Addr) -> Byte {
        self.ram[addr as usize]
    }

    fn readw(&self, addr: Addr) -> Word {
        let lo = self.readb(addr);
        let hi = self.readb(addr+1);
        (hi as Word) << 8 | lo as Word
    }

    fn writeb(&mut self, addr: Addr, data: Byte) {
        self.ram[addr as usize] = data;
    }
}
