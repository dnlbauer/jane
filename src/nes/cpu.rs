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
            sp: 0x00FD,
            pc: 0x0000,
            flags: 0x0034,
        }
    }
}

impl Debug for Registers {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{{ a: {:#x}, x: {:#x}, y: {:#x}, sp: {:#x}, pc: {:#x}, flags: {:#010b} }}",
            self.a, self.x, self.y, self.sp, self.pc, self.flags)
    } 
}

pub const STACK_BASE_ADDR: Addr = 0x0100;

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
    fn clock<T: Memory>(&mut self, bus: &mut T) {
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

    // sets PC 
    pub fn find_pc_addr<T: Memory>(&mut self, bus: &T) {
        // 0xfffc and 0xfffc+1 stores the location of the first op code (where
        // the program starts). Read it and set pc accordingly. 
        let addr: u16 = 0xfffc;
        let lo = bus.readb(addr);
        let hi = bus.readb(addr + 1);
        self.regs.pc = (hi as u16) << 8 | lo as u16;
        debug!("PC set to: {:#06x}", self.regs.pc);
    }

    // Brings the CPU to a known state. Resets all registers and flags
    // Read location of pc from 0xfffc
    pub fn reset<T: Memory>(&mut self, bus: &T) {
        // reset registers
        self.regs.sp = self.regs.sp - 3;
        self.set_flag(IRQ, true);

        self.find_pc_addr(bus);

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
    fn readb_pc<T: Memory>(&mut self, bus: &T) -> Byte {
        let val = self.readb(bus, self.regs.pc);
        self.regs.pc += 1;
        val
    }

    // read whole Word from pc
    fn readw_pc<T: Memory>(&mut self, bus: &T) -> Word {
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

    fn run_instruction<T: Memory>(&mut self, bus: &mut T, i: &Instruction) -> u8 {
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
            Operation::INX => self.op_INX(bus),
            Operation::INY => self.op_INY(bus),
            Operation::JMP => self.op_JMP(bus, value),
            Operation::JSR => self.op_JSR(bus, value),
            Operation::LDA => self.op_LDA(bus, value),
            Operation::LDX => self.op_LDX(bus, value),
            Operation::LDY => self.op_LDY(bus, value),
            Operation::LSR => self.op_LSR(bus, value),
            Operation::NOP => self.op_NOP(bus, value),
            Operation::ORA => self.op_ORA(bus, value),
            Operation::PHA => self.op_PHA(bus, value),
            Operation::PHP => self.op_PHP(bus),
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
            Operation::TSX => self.op_TSX(bus),
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
    fn am_IMP<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
       (0, false)
    }

    // Immediate, next byte comes from pc
    fn am_IMM<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let addr = self.regs.pc;
        self.regs.pc += 1;
        (addr, false)
    }

    // Absolute address on zero page
    fn am_ZP0<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let addr = self.readb_pc(bus);
        (addr as Word & 0x0ff, false) 
    }

    // Absolute address on zero page with x offset
    fn am_ZPX<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let addr = self.readb_pc(bus) + self.regs.x;
        (addr as Word & 0x0ff, false)
    }

    // Absolute address on zero page with y offset
    fn am_ZPY<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let addr = self.readb_pc(bus) + self.regs.y;
        (addr as Word & 0x0ff, false)
    }

    fn am_REL<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let rel_addr = self.readb_pc(bus) as Word;
        if rel_addr < 0x80 {
            (self.regs.pc + rel_addr, false)    
        } else {
            (self.regs.pc + rel_addr - 256, false)
        }
    }

    fn am_ABS<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let addr = self.readw_pc(bus);
        (addr, false)
    }

    fn am_ABX<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let addr = self.readw_pc(bus) + self.regs.x as Word;
        (addr, false)  
    }

    fn am_ABY<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let addr = self.readw_pc(bus) + self.regs.y as Word;
        (addr, false)  
    }

    // the next 16 bits are an address. This address stores the real address
    // that is used for the operation.
    // Hardware bug: Normally, if lo of the supplied address is 0xFF, high byte
    // must be read from the next page. Instead it wraps around and reads from
    // the same page!
    fn am_IND<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let mut ind_addr = self.readw_pc(bus);
        
        let lo = ind_addr & 0b00001111;
        if lo == 0x00FF {
            ind_addr -= 0x00FF; 
        } 


        let addr = bus.readw(ind_addr);
        (addr, false)
    }

    // the next 16 bits + x are an address. This address stores the real address
    // that is used for the operation.
    // Hardware bug: Normally, if lo of the supplied address is 0xFF, high byte
    // must be read from the next page. Instead it wraps around and reads from
    // the same page!
    fn am_IZX<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let mut ind_addr = self.readb_pc(bus) as Word;
        ind_addr += self.regs.x as Word;

        let lo = ind_addr & 0b00001111;
        if lo == 0x00FF {
            ind_addr -= 0x00FF; 
        } 


        let addr = bus.readw(ind_addr);
        (addr, false)  
    }

    // the next 16 bits + y are an address. This address stores the real address
    // that is used for the operation.
    // Hardware bug: Normally, if lo of the supplied address is 0xFF, high byte
    // must be read from the next page. Instead it wraps around and reads from
    // the same page!
    fn am_IZY<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let mut ind_addr = self.readb_pc(bus) as Word;
        ind_addr += self.regs.y as Word;

        let lo = ind_addr & 0b00001111;
        if lo == 0x00FF {
            ind_addr -= 0x00FF; 
        } 


        let addr = bus.readw(ind_addr);
        (addr, false)  
    }

    // Operations

    // Add to A
    fn op_ADC<T: Memory>(&mut self, bus: &T, addr: Word) {
        let val = self.readb(bus, addr) as Word;
        let tmp = (self.regs.a as Word) + val + (self.get_flag(CARRY) as Word);
        self.set_flag(CARRY, (tmp & 0xFF) > 255);
        self.set_flag(ZERO, tmp == 0);
        self.set_flag(NEGATIVE, (tmp & 0x80) == 1);
        self.set_flag(OVERFLOW, !(((self.regs.a as Word) ^ tmp) & !((self.regs.a as Word) ^ val) & 0x80) == 1);
        self.regs.a = tmp as Byte;
    }

    fn op_AND<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_ASL<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BCC<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BCS<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BEQ<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BIT<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BMI<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    // Jump if 0
    fn op_BNE<T: Memory>(&mut self, bus: &T, addr: Word) {
        if self.get_flag(ZERO) == 0 {
            let old_addr = self.regs.pc;
            self.regs.pc = addr;
            println!("Jumping from {:#x} to {:#x}", old_addr, addr);
        }
    }

    fn op_BPL<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BRK<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BVC<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_BVS<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    // Clear carry flag
    fn op_CLC(&mut self) {
        self.set_flag(CARRY, false);
    }
   
    // clear decimal flag
    fn op_CLD<T: Memory>(&mut self, bus: &T, val: Word) {
        self.set_flag(DECIMAL, false);
    }


    // clear IRQ
    fn op_CLI<T: Memory>(&mut self, bus: &T, val: Word) {
        self.set_flag(IRQ, false);
    }

    // clear Overflow
    fn op_CLV<T: Memory>(&mut self, bus: &T, val: Word) {
        self.set_flag(OVERFLOW, false);
    }

    fn op_CMP<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_CPX<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_CPY<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_DEC<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    // Decrement X
    fn op_DEX(&mut self) {
        self.regs.x -= 1;
        self.set_flag(ZERO, self.regs.x == 0);
        self.set_flag(NEGATIVE, (self.regs.x & 0x80) != 0)
    }

    // Decrement Y
    fn op_DEY(&mut self) {
        self.regs.y -= 1;
        self.set_flag(ZERO, self.regs.y == 0);
        self.set_flag(NEGATIVE, (self.regs.y & 0x80) != 0)
    }

    fn op_EOR<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_INC<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_INX<T: Memory>(&mut self, bus: &T) {
        self.regs.x += 1;
        self.set_flag(ZERO, self.regs.x == 0);
        self.set_flag(NEGATIVE, (self.regs.x & 0x80) != 0)
    }

    fn op_INY<T: Memory>(&mut self, bus: &T) {
        self.regs.y += 1;
        self.set_flag(ZERO, self.regs.y == 0);
        self.set_flag(NEGATIVE, (self.regs.y & 0x80) != 0)
    }

    // Jump to address (set pc)
    fn op_JMP<T: Memory>(&mut self, bus: &T, addr: Word) {
        self.regs.pc = addr;
    }

    // Jump to subroutine (leaves trace on the stack)
    fn op_JSR<T: Memory>(&mut self, bus: &mut T, addr: Word) {
        self.regs.pc -= 1;
        bus.writeb(STACK_BASE_ADDR + self.regs.sp as Word, ((self.regs.pc >> 8) & 0x00ff) as Byte);
        self.regs.sp -= 1;
        bus.writeb(STACK_BASE_ADDR + self.regs.sp as Word, (self.regs.pc & 0x00ff) as Byte);
        self.regs.sp -= 1;
        self.regs.pc = addr;
    }

    // Read value from addr into A
    fn op_LDA<T: Memory>(&mut self, bus: &T, addr: Word) {
        let val = bus.readb(addr);
        self.regs.a = val;
        self.set_flag(ZERO, val == 0);
        self.set_flag(NEGATIVE, (val & 0x80) != 0);
    }

    // Read value from addr into X
    fn op_LDX<T: Memory>(&mut self, bus: &T, addr: Word) {
        let val = bus.readb(addr);
        self.regs.x = val;
        self.set_flag(ZERO, val == 0);
        self.set_flag(NEGATIVE, (val & 0x80) != 0);
    }

    // Read value from addr into Y
    fn op_LDY<T: Memory>(&mut self, bus: &T, addr: Word) {
        let val = bus.readb(addr);
        self.regs.y = val;
        self.set_flag(ZERO, val == 0);
        self.set_flag(NEGATIVE, (val & 0x80) != 0);
    }

    fn op_LSR<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_NOP<T: Memory>(&mut self, bus: &T, val: Word) {
        // does nothing
    }

    fn op_ORA<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_PHA<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    // Write flags to stack
    fn op_PHP<T: Memory>(&mut self, bus: &mut T) {
        let tmp = self.regs.flags | BREAK | UNUSED;
        bus.writeb(STACK_BASE_ADDR + self.regs.sp as Word, tmp);
        self.set_flag(BREAK, false);
        self.set_flag(UNUSED, false);
        self.regs.sp -= 1;
        println!("PHP executed");
    }

    // Read from stack into A
    fn op_PLA<T: Memory>(&mut self, bus: &T, val: Word) {
        self.regs.sp += 1;
        self.regs.a = bus.readb(STACK_BASE_ADDR + self.regs.sp as Word);
        self.set_flag(ZERO, self.regs.a == 0);
        self.set_flag(NEGATIVE, (self.regs.a & 0x80) == 1)
    }

    fn op_PLP<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_ROL<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_ROR<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_RTI<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_RTS<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_SBC<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_SEC<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    fn op_SED<T: Memory>(&mut self, bus: &T, val: Word) {
        unimplemented!()
    }

    // set irq flag
    fn op_SEI<T: Memory>(&mut self, bus: &T, val: Word) {
        self.set_flag(IRQ, true);
    }

    // Push A reg to memory
    fn op_STA<T: Memory>(&mut self, bus: &mut T, addr: Word) {
        self.writeb(bus, addr, self.regs.a)
    }

    // Push X reg to memory
    fn op_STX<T: Memory>(&mut self, bus: &mut T, addr: Word) {
        self.writeb(bus, addr, self.regs.x)
    }

    // Push Y reg to memory
    fn op_STY<T: Memory>(&mut self, bus: &mut T, addr: Word) {
        self.writeb(bus, addr, self.regs.y)
    }

    // a to x
    fn op_TAX<T: Memory>(&mut self, bus: &T, val: Word) {
        self.regs.x = self.regs.a;
        self.set_flag(ZERO, self.regs.x == 0);
        self.set_flag(NEGATIVE, (self.regs.x & 0x80) == 1)
    }

    // a to y
    fn op_TAY<T: Memory>(&mut self, bus: &T, val: Word) {
        self.regs.y = self.regs.a;
        self.set_flag(ZERO, self.regs.y == 0);
        self.set_flag(NEGATIVE, (self.regs.y & 0x80) == 1)
    }

    // stack pointer to x
    fn op_TSX<T: Memory>(&mut self, bus: &T) {
        self.regs.x = self.regs.sp;
        self.set_flag(ZERO, self.regs.x == 0);
        self.set_flag(NEGATIVE, (self.regs.x & 0x80) == 1)
    }

    // transfer x to a
    fn op_TXA<T: Memory>(&mut self, bus: &T, val: Word) {
        self.regs.a = self.regs.x;
        self.set_flag(ZERO, self.regs.a == 0);
        self.set_flag(NEGATIVE, (self.regs.a & 0x80) == 1)
    }

    // transfer y to a
    fn op_TYA<T: Memory>(&mut self, bus: &T, val: Word) {
        self.regs.a = self.regs.y;
        self.set_flag(ZERO, self.regs.a == 0);
        self.set_flag(NEGATIVE, (self.regs.a & 0x80) == 1)
    }   

    // transfer x to stack
    fn op_TXS<T: Memory>(&mut self, bus: &T, val: Word) {
        self.regs.sp = self.regs.x;
    } 
}

impl Debug for CPU {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:?}, op: {:x}, cycle: {:?}",
            self.regs, self.curr_op, self.cycles)
    } 
}


