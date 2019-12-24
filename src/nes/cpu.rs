pub mod instructions;

use instructions::{Instruction,Operation,AddrMode};
use core::fmt::{Debug,Formatter,Result};
use crate::nes::bus::*;
use crate::nes::types::*;
use log::{debug};

pub struct Registers {
    pub a: Byte,
    pub x: Byte,
    pub y: Byte,
    pub sp: Byte,
    pub pc: Addr,
    pub flags: Byte 
}

impl Registers {
    pub fn new() -> Registers {
        // TODO true initial state of registers before reset?
        Registers {
            a: 0,
            x: 0,
            y: 0,
            sp: 0,
            pc: 0,
            flags: 0,
        }
    }
}

impl Debug for Registers {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{{ a: {:#x}, x: {:#x}, y: {:#x}, sp: {:#x}, pc: {:#x}, flags: {:#010b} }}",
            self.a, self.x, self.y, self.sp, self.pc, self.flags)
    } 
}


pub const CARRY: Byte = 1 << 0;
pub const ZERO: Byte = 1 << 1;
pub const IRQ: Byte = 1 << 2;
pub const DECIMAL: Byte = 1 << 3;
pub const BREAK: Byte = 1 << 4;
pub const UNUSED: Byte = 1 << 5;
pub const OVERFLOW: Byte = 1 << 6;
pub const NEGATIVE: Byte = 1 << 7;

pub struct CPU {
    pub regs: Registers,
    curr_op: Byte,  // current operation
    cycles: u8,  // number of clock clycles the CPU is ahead of global clock
}

// Default implementation to read/write from bus
impl BusDevice for CPU { }

// A cpu is clockable
impl Clockable for CPU {
    fn clock<T: Bus>(&mut self, bus: &mut T) {
        if self.cycles == 0 {
            let opcode = self.readb_pc(bus);
            self.curr_op = opcode;
            let instruction = Instruction::decode_op(opcode).unwrap();  // TODO error handling
            self.cycles = self.run_instruction(bus, instruction);
        }
        debug!("{:?}", self);
        self.cycles -= 1;

    }
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            regs: Registers::new(),
            cycles: 0,
            curr_op: 0x00,
        }
    }

    // Brings the CPU to a known state. Resets all registers and flags
    // Read location of pc from 0xfffc
    pub fn reset<T: Bus>(&mut self, bus: &T) {
        // reset registers
        self.regs.a = 0;
        self.regs.x = 0;
        self.regs.y = 0;
        self.regs.sp = 0xfd;
        self.regs.pc = 0x00;
        self.regs.flags = 0x00 | UNUSED;

        // 0xfffc and 0xfffc+1 stores the location of the first op code (where
        // the program starts). Read it and set pc accordingly. 
        let addr: u16 = 0xfffc;
        let lo = bus.readb(addr);
        let hi = bus.readb(addr + 1);
        self.regs.pc = (hi as u16) << 8 | lo as u16;

        // A reset takes 8 CPU clocks
        self.cycles = 8;

        // reset internal variables
        self.curr_op = 0x00;
    }

    // True if the operation is not finished yet
    pub fn is_ahead(&self) -> bool {
        return self.cycles > 0;
    }

    // read the next opcode and increment pc
    fn readb_pc<T: Bus>(&mut self, bus: &T) -> Byte {
        let val = self.readb(bus, self.regs.pc);
        self.regs.pc += 1;
        val
    }

    // read whole Word from pc
    fn readw_pc<T: Bus>(&mut self, bus: &T) -> Word {
        let val = self.readw(bus, self.regs.pc);
        self.regs.pc += 2;
        val
    }

    // Set the flag with the corresponding mask
    fn set_flag(&mut self, flag: Byte, val: bool) {
        debug!("{}set flag: {:08b}", if val { "" } else {"un"}, flag);
        if val {
            self.regs.flags |= flag;
        } else {
            self.regs.flags &= !flag;
        } 
    }

    pub fn get_flag(&self, flag: Byte) -> Byte {
        if self.regs.flags & flag > 0 {
            1
        } else {
            0
        }
    }

    fn run_instruction<T: Bus>(&mut self, bus: &mut T, i: &Instruction) -> u8 {
        let (value, page_cross) = match &i.addr_mode {
            AddrMode::IMP => self.am_IMP(bus),
            AddrMode::IMM => self.am_IMM(bus),
            AddrMode::ZP0 => self.am_ZP0(bus),
            AddrMode::ZPX => self.am_ZPX(bus),
            AddrMode::ZPY => self.am_ZPY(bus),
            AddrMode::REL => self.am_REL(bus),
            AddrMode::ABS => self.am_ABS(bus),
            AddrMode::ABX => self.am_ABX(bus),
            AddrMode::ABY => self.am_ABY(bus),
            AddrMode::IND => self.am_IND(bus),
            AddrMode::IZX => self.am_IZX(bus),
            AddrMode::IZY => self.am_IZY(bus),
        };
        debug!("{:?}, Operand: {:#x}", i, value);

        match i.operation {
            Operation::ADC => self.op_ADC(bus, value),
            Operation::AND => self.op_AND(bus, value),
            Operation::ASL => self.op_ASL(bus, value),
            Operation::BCC => self.op_BCC(bus, value),
            Operation::BCS => self.op_BCS(bus, value),
            Operation::BEQ => self.op_BEQ(bus, value),
            Operation::BIT => self.op_BIT(bus, value),
            Operation::BMI => self.op_BMI(bus, value),
            Operation::BNE => self.op_BNE(bus, value),
            Operation::BPL => self.op_BPL(bus, value),
            Operation::BRK => self.op_BRK(bus, value),
            Operation::BVC => self.op_BVC(bus, value),
            Operation::BVS => self.op_BVS(bus, value),
            Operation::CLC => self.op_CLC(),
            Operation::CLD => self.op_CLD(bus, value),
            Operation::CLI => self.op_CLI(bus, value),
            Operation::CLV => self.op_CLV(bus, value),
            Operation::CMP => self.op_CMP(bus, value),
            Operation::CPX => self.op_CPX(bus, value),
            Operation::CPY => self.op_CPY(bus, value),
            Operation::DEC => self.op_DEC(bus, value),
            Operation::DEX => self.op_DEX(),
            Operation::DEY => self.op_DEY(),
            Operation::EOR => self.op_EOR(bus, value),
            Operation::INC => self.op_INC(bus, value),
            Operation::INX => self.op_INX(bus, value),
            Operation::INY => self.op_INY(bus, value),
            Operation::JMP => self.op_JMP(bus, value),
            Operation::JSR => self.op_JSR(bus, value),
            Operation::LDA => self.op_LDA(bus, value),
            Operation::LDX => self.op_LDX(bus, value),
            Operation::LDY => self.op_LDY(bus, value),
            Operation::LSR => self.op_LSR(bus, value),
            Operation::NOP => self.op_NOP(bus, value),
            Operation::ORA => self.op_ORA(bus, value),
            Operation::PHA => self.op_PHA(bus, value),
            Operation::PHP => self.op_PHP(bus, value),
            Operation::PLA => self.op_PLA(bus, value),
            Operation::PLP => self.op_PLP(bus, value),
            Operation::ROL => self.op_ROL(bus, value),
            Operation::ROR => self.op_ROR(bus, value),
            Operation::RTI => self.op_RTI(bus, value),
            Operation::RTS => self.op_RTS(bus, value),
            Operation::SBC => self.op_SBC(bus, value),
            Operation::SEC => self.op_SEC(bus, value),
            Operation::SED => self.op_SED(bus, value),
            Operation::SEI => self.op_SEI(bus, value),
            Operation::STA => self.op_STA(bus, value),
            Operation::STX => self.op_STX(bus, value),
            Operation::STY => self.op_STY(bus, value),
            Operation::TAX => self.op_TAX(bus, value),
            Operation::TAY => self.op_TAY(bus, value),
            Operation::TSX => self.op_TSX(bus, value),
            Operation::TXA => self.op_TXA(bus, value),
            Operation::TXS => self.op_TXS(bus, value),
            Operation::TYA => self.op_TYA(bus, value),
        }

        if page_cross {
            i.cycles[0] + i.cycles[1]
        } else {
            i.cycles[0]
        }
    }

        // Implied aka no target
    fn am_IMP<T: Bus>(&mut self, bus: &T) -> (Word, bool) {
       (0, false)
    }

    // Immediate, next byte comes from pc
    fn am_IMM<T: Bus>(&mut self, bus: &T) -> (Word, bool) {
        let addr = self.regs.pc;
        self.regs.pc += 1;
        (addr, false)
    }

    fn am_ZP0<T: Bus>(&mut self, bus: &T) -> (Word, bool) {
        unimplemented!()  
    }

    fn am_ZPX<T: Bus>(&mut self, bus: &T) -> (Word, bool) {
        unimplemented!()  
    }

    fn am_ZPY<T: Bus>(&mut self, bus: &T) -> (Word, bool) {
        unimplemented!()  
    }

    fn am_REL<T: Bus>(&mut self, bus: &T) -> (Word, bool) {
        let rel_addr = self.readb_pc(bus) as Word;
        if rel_addr < 0x80 {
            (self.regs.pc + rel_addr, false)    
        } else {
            (self.regs.pc + rel_addr - 256, false)
        }
    }

    fn am_ABS<T: Bus>(&mut self, bus: &T) -> (Word, bool) {
        let addr = self.readw_pc(bus);
        (addr, false)
    }

    fn am_ABX<T: Bus>(&mut self, bus: &T) -> (Word, bool) {
        unimplemented!()  
    }

    fn am_ABY<T: Bus>(&mut self, bus: &T) -> (Word, bool) {
        unimplemented!()  
    }

    fn am_IND<T: Bus>(&mut self, bus: &T) -> (Word, bool) {
        unimplemented!()  
    }

    fn am_IZX<T: Bus>(&mut self, bus: &T) -> (Word, bool) {
        unimplemented!()  
    }

    fn am_IZY<T: Bus>(&mut self, bus: &T) -> (Word, bool) {
        unimplemented!()  
    } 

    // Operations
    fn op_ADC<T: Bus>(&mut self, bus: &T, addr: Word) {
        let val = self.readb(bus, addr) as Word;
        let tmp = (self.regs.a as Word) + val + (self.get_flag(CARRY) as Word);
        self.set_flag(CARRY, (tmp & 0xFF) > 255);
        self.set_flag(ZERO, tmp == 0);
        self.set_flag(NEGATIVE, (tmp & 0x80) == 1);
        self.set_flag(OVERFLOW, !(((self.regs.a as Word) ^ tmp) & !((self.regs.a as Word) ^ val) & 0x80) == 1);
        self.regs.a = tmp as Byte;
    }

    fn op_AND<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_ASL<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BCC<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BCS<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BEQ<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BIT<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BMI<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BNE<T: Bus>(&mut self, bus: &T, addr: Word) {
        if self.get_flag(ZERO) == 0 {
            let old_addr = self.regs.pc;
            self.regs.pc = addr;
            println!("Jumping from {:#x} to {:#x}", old_addr, addr);
        }
    }

    fn op_BPL<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BRK<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BVC<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BVS<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_CLC(&mut self) {
        self.set_flag(CARRY, false);
    }

    fn op_CLD<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_CLI<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_CLV<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_CMP<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_CPX<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_CPY<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_DEC<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_DEX(&mut self) {
        self.regs.x -= 1;
        self.set_flag(ZERO, self.regs.x == 0);
        self.set_flag(NEGATIVE, (self.regs.x & 0x80) != 0)
    }

    fn op_DEY(&mut self) {
        self.regs.y -= 1;
        self.set_flag(ZERO, self.regs.y == 0);
        self.set_flag(NEGATIVE, (self.regs.y & 0x80) != 0)
    }

    fn op_EOR<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_INC<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_INX<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_INY<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_JMP<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_JSR<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_LDA<T: Bus>(&mut self, bus: &T, addr: Word) {
        let val = bus.readb(addr);
        self.regs.a = val;
        self.set_flag(ZERO, val == 0);
        self.set_flag(NEGATIVE, (val & 0x80) != 0);
    }

    fn op_LDX<T: Bus>(&mut self, bus: &T, addr: Word) {
        let val = bus.readb(addr);
        self.regs.x = val;
        self.set_flag(ZERO, val == 0);
        self.set_flag(NEGATIVE, (val & 0x80) != 0);
    }

    fn op_LDY<T: Bus>(&mut self, bus: &T, addr: Word) {
        let val = bus.readb(addr);
        self.regs.y = val;
        self.set_flag(ZERO, val == 0);
        self.set_flag(NEGATIVE, (val & 0x80) != 0);
    }

    fn op_LSR<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_NOP<T: Bus>(&mut self, bus: &T, val: Word) {
        // does nothing
    }

    fn op_ORA<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_PHA<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_PHP<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_PLA<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_PLP<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_ROL<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_ROR<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_RTI<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_RTS<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_SBC<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_SEC<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_SED<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_SEI<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_STA<T: Bus>(&mut self, bus: &mut T, addr: Word) {
        self.writeb(bus, addr, self.regs.a)
    }

    fn op_STX<T: Bus>(&mut self, bus: &mut T, addr: Word) {
        self.writeb(bus, addr, self.regs.x)
    }

    fn op_STY<T: Bus>(&mut self, bus: &mut T, addr: Word) {
        self.writeb(bus, addr, self.regs.y)
    }

    fn op_TAX<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_TAY<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_TSX<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_TXA<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_TXS<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_TYA<T: Bus>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }    
}

impl Debug for CPU {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:?}, op: {:x}, cycle: {:?}",
            self.regs, self.curr_op, self.cycles)
    } 
}


