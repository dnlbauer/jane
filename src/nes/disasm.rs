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
        let mut mem_iter = ((start as usize) .. (stop as usize)+1).map({|a| a as Addr});
        while let Some(addr) = mem_iter.next() {

            let opcode = mem.readb(addr);
            
            // Decode opcode, default to NOP/IMP to "skip" the byte
            let i = Instruction::decode_op(opcode)
                .unwrap_or(Instruction::decode_op(0xeau8).unwrap()); 
            debug!("Disassembling opcode {:#06x}, {:?}", addr, i); 
            
            // debug!("{:?}", i);
            let args = match i.addr_mode {
                AddrMode::IMM => {
                    match mem_iter.next() {
                        Some(val) => {
                            let val = mem.readb(val);
                            format!("#{0:02x} ({0})", val)
                        },
                        None => continue,
                    }
                },
                AddrMode::ZP0 | AddrMode::ZPX | AddrMode::ZPY  => {
                    match mem_iter.next() {
                        Some(val) => {
                            let rel_addr = mem.readb(val);
                            format!("{:#04x}", rel_addr)
                        }
                        None => continue,
                    }
                },
                AddrMode::ABS | AddrMode::ABX | AddrMode::ABY => {
                    match mem_iter.next() {
                        Some(val) => {
                            let val = mem.readw(val);
                            mem_iter.next().unwrap();
                            format!("{:#06x}", val)
                        },
                        None => continue,
                    }
                },
                AddrMode::REL => {
                    match mem_iter.next() {
                        Some(val) => {
                            let rel_addr = mem.readb(val) as Word;
                            let jmp_addr = Disasm::get_rel_addr(rel_addr, addr+2);
                            format!("#{:02x} => {:#06x}", rel_addr, jmp_addr)
                        },
                        None => continue,
                    }
                },
                AddrMode::IND => {
                    match mem_iter.next() {
                        Some(val) => {
                            let addr = mem.readw(val);
                            mem_iter.next().unwrap();
                            format!("{:#06x}", addr)
                        },
                        None => continue,
                    }
                },
                AddrMode::IZX | AddrMode::IZY => {
                    match mem_iter.next() {
                        Some(val) => {
                            let addr = mem.readb(val);
                            format!("{:#06x}", addr)
                        },
                        None => continue,
                    }

                }
                AddrMode::IMP => String::from(""),
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