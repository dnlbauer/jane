use crate::nes::cpu::instructions::*;
use failure::Error;
use std::fmt;
use crate::nes::bus::*;
use crate::nes::types::*;
use std::fmt::Debug;

// Disassemble code around the pc of the cpu
pub struct Disasm {
    pub start: Addr,
    pub stop: Addr,
    pub instructions: Vec<String>,
    pub addresses: Vec<Addr>
}

impl BusDevice for Disasm {}

impl Disasm {
    // Disassemble given code region
    pub fn disassemble(mem: &MemoryBus, start: Addr, stop: Addr) -> Result<Self, Error> {
        let mut instructions = Vec::new();
        let mut addresses = Vec::new();
        let mut mem_iter = ((start as usize) .. (stop as usize)).map({|a| a as Addr});
        while let Some(addr) = mem_iter.next() {

            let opcode = mem.readb(addr);
            
            
            // Decode opcode, default to NOP/IMP to "skip" the byte
            let i = Instruction::decode_op(opcode)
                .unwrap_or(Instruction::decode_op(0xeau8).unwrap()); 
            
            // debug!("{:?}", i);
            let args = match i.addr_mode {
                AddrMode::IMM => {
                    let val = mem.readb(mem_iter.next().unwrap());
                    format!("#{0:02x} ({0})", val)
                },
                AddrMode::ZP0 | AddrMode::ZPX | AddrMode::ZPY  => {
                    let rel_addr = mem.readb(mem_iter.next().unwrap());
                    format!("{:#04x}", rel_addr)
                },
                AddrMode::ABS | AddrMode::ABX | AddrMode::ABY => {
                    let val = mem.readw(mem_iter.next().unwrap());
                    mem_iter.next().unwrap();
                    format!("{:#06x}", val)
                },
                AddrMode::REL => { 
                    let rel_addr = mem.readb(mem_iter.next().unwrap()) as Word;
                    let jmp_addr = Disasm::get_rel_addr(rel_addr, addr+2);
                    format!("#{:02x} => {:#06x}", rel_addr, jmp_addr)
                },
                AddrMode::IND => {
                    let addr = mem.readw(mem_iter.next().unwrap());
                    mem_iter.next().unwrap();
                    format!("{:#06x}", addr)
                },
                AddrMode::IZX | AddrMode::IZY => {
                    let addr = mem.readb(mem_iter.next().unwrap());
                    format!("{:#06x}", addr)
                }
                AddrMode::IMP => String::from(""),
                _ => bail!("Disassembly of {} not implemented", i.addr_mode),
            };

            let s = format!("{:#06x}: {} {} ({})", addr, i.operation, args, i.addr_mode);
            // println!("opcode: {:#04x} {:?}", opcode, i);
            // println!("{}", &s);

            instructions.push(s);
            addresses.push(addr);

                        
        }
        Ok(Disasm { start, stop, instructions, addresses })
    }

    fn get_rel_addr(rel_addr: Addr, curr_addr: Addr) -> Addr {
        if rel_addr < 0x80 {
            curr_addr + rel_addr
        } else {
            curr_addr + (rel_addr) - 256
        }
    }

}


impl Debug for Disasm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}