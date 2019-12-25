use crate::nes::types::*;

pub trait Mapper {
    fn map_read_addr(&self, addr: Addr) -> Addr;
    fn map_write_addr(&self, addr: Addr) -> Addr;
}

// Mapper 0
// prg rom: 16K or 32K
// chr rom: 8K
// All banks are fixed:
// CPU:
//     0x8000 - 0xbfff // first 16k
//     0xc000 - 0xffff // second 16k
//     
// Cartrige:
// 0x4000-0xffff -> first 16K 
// 0xc000-0xffff -> last 16K or mirror or 0x8000-0xbfff
pub struct Mapper0 {
    prg_banks: Byte,
    chr_banks: Byte, 
}

impl Mapper0 {
    pub fn new(prg_banks: Byte, chr_banks: Byte) -> Self {
        Mapper0 { prg_banks, chr_banks }
    }
}

impl Mapper for Mapper0 {
    fn map_read_addr(&self, addr: Addr) -> Addr {
        if self.prg_banks > 1 {
            addr & 0x7fff
        } else {
            addr & 0x3fff
        }
    }
    fn map_write_addr(&self, addr: Addr) -> Addr {
        if self.prg_banks > 1 {
            addr & 0x7fff
        } else {
            addr & 0x3fff
        }
    }
}
