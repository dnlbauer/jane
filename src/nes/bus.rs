use crate::nes::types::*;
use crate::nes::cartridge::Cartridge;

const RAM_ADDR_RANGE: [Addr; 2] = [0x0000, 0x1fff];
const RAM_PHYS_RANGE: [Addr; 2] = [0x0000, 0x07ff];
const CART_ADDR_RANGE: [Addr; 2] = [0x4020, 0xffff];

// Generic interface for a device allowing to read/write memory
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

// Impl by devices to access the Bus
pub trait BusDevice {
    fn readb<T: Memory>(&self, bus: &T, addr: Addr) -> Byte {
        bus.readb(addr)
    }

    fn readw<T: Memory>(&self, bus: &T, addr: Addr) -> Word {
        bus.readw(addr)
    }

    fn writeb<T: Memory>(&mut self, bus: &mut T, addr: Addr, data: Byte) {
        bus.writeb(addr, data)
    }
}

// Impl by devices to do stuff on bus clock
pub trait Clockable {
    fn clock<T: Memory>(&mut self, bus: &mut T);
}


// A simple bus giving access to a chunk of memory
// and the cartrige
pub struct MemoryBus {
    ram: [Byte; 0x07ff],  // 2kb
    cartrige: Option<Cartridge>
}

impl MemoryBus {
    pub fn new() -> MemoryBus {
        MemoryBus {
            ram: [0; 0x07ff],
            cartrige: None,
        }
    }

    pub fn insert_cartrige(&mut self, cartrige: Cartridge) {
        self.cartrige = Some(cartrige);
    }
}

impl Memory for MemoryBus {
    
    fn readb(&self, addr: Addr) -> Byte {
        if let Some(cartrige) = &self.cartrige {
            if CART_ADDR_RANGE[0] <= addr && addr < CART_ADDR_RANGE[1] {
                return cartrige.readb(addr)
            } 
        }

        if RAM_ADDR_RANGE[0] <= addr && addr < RAM_ADDR_RANGE[1] {
            // Ram is 3x mirrored after 07ff
            return self.ram[(addr & 0x07ff) as usize]
        }
        0x0000  // generic response
    }

    fn writeb(&mut self, addr: Addr, data: Byte) {
        if let Some(cartrige) = &mut self.cartrige {
            if CART_ADDR_RANGE[0] <= addr && addr < CART_ADDR_RANGE[1] {
                cartrige.writeb(addr, data)
            } 
        }

        if RAM_ADDR_RANGE[0] <= addr && addr < RAM_ADDR_RANGE[1] {
            // Ram is 3x mirrored after 07ff
            self.ram[(addr & 0x07ff) as usize] = data
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_consts() {
        assert_eq!(RAM_ADDR_RANGE[1] - RAM_ADDR_RANGE[0] + 1, 2048*4);
        assert_eq!(RAM_PHYS_RANGE[1] - RAM_PHYS_RANGE[0] + 1, 2048);
           
    }

    #[test]
    fn test_read_write_ram() {
        let mut bus = MemoryBus::new();

        // read/write to ram addr
        bus.writeb(0x0000, 1);
        assert_eq!(1, bus.readb(0x0000));
        assert_eq!(1, bus.readb(0x0800));

        // read/write to mirrored ram
        bus.writeb(0x0801, 2);
        assert_eq!(2, bus.readb(0x0801));
        assert_eq!(2, bus.readb(0x0001));
    }

    #[test]
    fn test_read_write_cartrige() {
        let mut bus = MemoryBus::new();

        // read/write non-existant to cartrige
        bus.writeb(0x4030, 3);
        assert_eq!(0, bus.readb(0x4030));

        let cartrige = Cartridge::dummy();
        bus.insert_cartrige(cartrige);
        bus.writeb(0x4030, 3);
        assert_eq!(3, bus.readb(0x4030));
    }


}