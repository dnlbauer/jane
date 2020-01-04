use crate::nes::types::*;
use crate::nes::cartridge::Cartridge;

const RAM_ADDR_RANGE: [Addr; 2] = [0x0000, 0x1fff];
const RAM_PHYS_RANGE: [Addr; 2] = [0x0000, 0x07ff];
const PPU_ADDR_RANGE: [Addr; 2] = [0x2000, 0x3fff];
const PPU_PHYS_RANGE: [Addr; 2] = [0x2000, 0x2007];
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

// PPU interface to allow read/write of memory
pub trait PPUMemory {
    fn readb_ppu(&self, addr: Addr) -> Byte;
    fn writeb_ppu(&mut self, addr: Addr, data: Byte);
    fn readw_ppu(&self, addr: Addr) -> Word {
        let lo = self.readb_ppu(addr);
        let hi = self.readb_ppu(addr+1);
        (hi as Word) << 8 | lo as Word
    }
    fn writew_ppu(&mut self, addr: Addr, data: Word) {
        self.writeb_ppu(addr, data as Byte);
        self.writeb_ppu(addr + 1, (data >> 8) as Byte);
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

// Impl by devices to access the Bus
pub trait PPUBusDevice {
    fn readb<T: PPUMemory>(&self, bus: &T, addr: Addr) -> Byte {
        bus.readb_ppu(addr)
    }

    fn readw<T: PPUMemory>(&self, bus: &T, addr: Addr) -> Word {
        bus.readw_ppu(addr)
    }

    fn writeb<T: PPUMemory>(&mut self, bus: &mut T, addr: Addr, data: Byte) {
        bus.writeb_ppu(addr, data)
    }
}

// Impl by devices to do stuff on bus clock
pub trait Clockable {
    fn clock<T: Memory>(&mut self, bus: &mut T);
}


// A simple bus giving access to a chunk of memory
// and the cartrige
pub struct Bus {
    ram: [Byte; 0x0800],  // 2kb
    cartrige: Option<Cartridge>
}

impl Bus {
    pub fn new() -> Bus {
        Bus {
            ram: [0; 0x0800],
            cartrige: None,
        }
    }

    pub fn insert_cartrige(&mut self, cartrige: Cartridge) {
        self.cartrige = Some(cartrige);
    }
}

impl Memory for Bus {

    fn readb(&self, addr: Addr) -> Byte {
        if let Some(cartrige) = &self.cartrige {
            if CART_ADDR_RANGE[0] <= addr && addr <= CART_ADDR_RANGE[1] {
                return cartrige.readb(addr)
            } 
        }

        if RAM_ADDR_RANGE[0] <= addr && addr <= RAM_ADDR_RANGE[1] {
            // Ram is 3x mirrored after 0x07ff
            return self.ram[(addr & RAM_PHYS_RANGE[1]) as usize]
        }
        // if PPU_ADDR_RANGE[0] <= addr && addr <= PPU_ADDR_RANGE[1] {
        //     // PPU memory is mirrored after 0x2007 to 0x3fff 
        //     return self.ram[(addr & PPU_PHYS_RANGE[1]) as usize]
        // }
        0x0000  // generic response
    }

    fn writeb(&mut self, addr: Addr, data: Byte) {
        if let Some(cartrige) = &mut self.cartrige {
            if CART_ADDR_RANGE[0] <= addr && addr <= CART_ADDR_RANGE[1] {
                cartrige.writeb(addr, data)
            } 
        }
        if RAM_ADDR_RANGE[0] <= addr && addr <= RAM_ADDR_RANGE[1] {
            // Ram is 3x mirrored after 07ff
            self.ram[(addr & RAM_PHYS_RANGE[1]) as usize] = data
        }
        // if PPU_ADDR_RANGE[0] <= addr && addr <= PPU_ADDR_RANGE[1] {
        //     // PPU memory is mirrored after 0x2007 to 0x3fff
        //     self.ram[(addr & PPU_PHYS_RANGE[1]) as usize] = data
        // }
    }
}

impl PPUMemory for Bus {
    fn readb_ppu(&self, addr: Addr) -> Byte {
        if let Some(cartrige) = &self.cartrige {
            if CART_ADDR_RANGE[0] <= addr && addr <= CART_ADDR_RANGE[1] {
                return cartrige.readb(addr)
            } 
        }

        if RAM_ADDR_RANGE[0] <= addr && addr <= RAM_ADDR_RANGE[1] {
            // Ram is 3x mirrored after 07ff
            return self.ram[(addr & 0x07ff) as usize]
        }
        0x0000  // generic response
    }

    fn writeb_ppu(&mut self, addr: Addr, data: Byte) {
        if let Some(cartrige) = &mut self.cartrige {
            if CART_ADDR_RANGE[0] <= addr && addr <= CART_ADDR_RANGE[1] {
                cartrige.writeb(addr, data)
            } 
        }

        if RAM_ADDR_RANGE[0] <= addr && addr <= RAM_ADDR_RANGE[1] {
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
        let mut bus = Bus::new();

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
        let mut bus = Bus::new();

        // read/write non-existant to cartrige
        bus.writeb(0x4030, 3);
        assert_eq!(0, bus.readb(0x4030));

        let cartrige = Cartridge::dummy();
        bus.insert_cartrige(cartrige);
        bus.writeb(0x4030, 3);
        assert_eq!(3, bus.readb(0x4030));
    }


}