use crate::nes::types::*;

// Generic interface describing a Bus
pub trait Bus {
    fn readb(&self, addr: Addr) -> Byte;
    fn readw(&self, addr: Addr) -> Word;
    fn writeb(&mut self, addr: Addr, data: Byte);
    fn writew(&mut self, addr: Addr, data: Word);
}

// Impl by devices to access the Bus
pub trait BusDevice {
    fn readb<T: Bus>(&self, bus: &T, addr: Addr) -> Byte {
        bus.readb(addr)
    }

    fn readw<T: Bus>(&self, bus: &T, addr: Addr) -> Word {
        bus.readw(addr)
    }

    fn writeb<T: Bus>(&mut self, bus: &mut T, addr: Addr, data: Byte) {
        bus.writeb(addr, data)
    }
}

// Impl by devices to do stuff on bus clock
pub trait Clockable {
    fn clock<T: Bus>(&mut self, bus: &mut T);
}


// A simple bus giving access to a chunk of memory
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

    fn writew(&mut self, addr: Addr, data: Word) {
        self.writeb(addr, data as Byte);
        self.writeb(addr + 1, (data >> 8) as Byte);
    }
}
