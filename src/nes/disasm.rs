use failure::Error;
use crate::nes::bus::*;
use crate::nes::types::*;
use crate::nes::cpu::instructions::*;

// Disassemble code around the pc of the cpu
pub struct Disasm {
    pub offset: Addr,
    pub instructions: Vec<String>,
}

impl BusDevice for Disasm {}
impl Clockable for Disasm {
    fn clock<T: Bus>(&mut self, bus: &mut T) { 
        // TODO find new pc location
    } 
}

impl Disasm {
    // Disassemble given code region
    pub fn disassemble(mem: &[Byte], offset: Addr) -> Result<Self, Error> {
        let mut instructions = Vec::new();
        let mut mem_iter = mem.iter().enumerate();
        while let Some(b) = mem_iter.next() {
            let addr = offset + (b.0 as Addr);
            let i = Instruction::decode_op(*b.1)?;

            let args = match i.addr_mode {
                AddrMode::IMM => format!("#{0:#04x} ({0})", mem_iter.next().unwrap().1),
                // AddrMode::ZP0 => self.am_ZP0(bus),
                // AddrMode::ZPX => self.am_ZPX(bus),
                // AddrMode::ZPY => self.am_ZPY(bus),
                AddrMode::ABS => {
                    let lo = *mem_iter.next().unwrap().1;
                    let hi = *mem_iter.next().unwrap().1;
                    let val = (hi as Addr) << 8 | lo as Addr;
                    format!("{:#06x}", val)
                    
                }
                AddrMode::REL => { 
                    let rel_addr = (*mem_iter.next().unwrap().1) as Addr;
                    let jmp_addr = Disasm::get_rel_addr(rel_addr, addr+2);
                    format!("{:#04x} => {:#06x}", rel_addr, jmp_addr)
                },
                
                // AddrMode::ABX => self.am_ABX(bus),
                // AddrMode::ABY => self.am_ABY(bus),
                // AddrMode::IND => self.am_IND(bus),
                // AddrMode::IZX => self.am_IZX(bus),
                // AddrMode::IZY => self.am_IZY(bus), 
                AddrMode::IMP => String::from(""),
                _ => String::from("TODO"),
            };

            let s = format!("{:#06x}: {} {} ({})", addr, i.operation, args, i.addr_mode);
            instructions.push(s);
        }
        Ok(Disasm { offset, instructions })
    }

    fn get_rel_addr(rel_addr: Addr, curr_addr: Addr) -> Addr {
        if rel_addr < 0x80 {
            curr_addr + rel_addr
        } else {
            curr_addr + (rel_addr) - 256
        }
    }

}