pub mod instructions;

use instructions::{Instruction,Operation,AddrMode};
use core::fmt::{Debug,Formatter,Result};
use crate::nes::bus::*;
use crate::nes::types::*;
use log::{debug};
use failure::err_msg;

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
            flags: 0x24,
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
pub const LO: Addr  = 0x00FF;
pub const HI: Addr  = 0xFFFF;

pub const CARRY: Byte = 1 << 0;    // 0000 0001 0x01
pub const ZERO: Byte = 1 << 1;     // 0000 0010 0x02
pub const IRQ: Byte = 1 << 2;      // 0000 0100 0x04
pub const DECIMAL: Byte = 1 << 3;  // 0000 1000 0x08
pub const BREAK: Byte = 1 << 4;    // 0001 0000 0x10
pub const UNUSED: Byte = 1 << 5;   // 0010 0000 0x20
pub const OVERFLOW: Byte = 1 << 6; // 0100 0000 0x40
pub const NEGATIVE: Byte = 1 << 7; // 1000 0000 0x80

pub struct CPU {
    pub regs: Registers,
    curr_op: Byte,  // current operation
    cycles: u64,  // number of clock clycles the CPU is ahead of global clock
    cycles_ahead: u8,
    stopped: bool, 
}

// Default implementation to read/write from bus
impl BusDevice for CPU { }

// A cpu is clockable
impl Clockable for CPU {
    fn clock<T: Memory>(&mut self, bus: &mut T) {
        // if processor is halted, we do nothing anymore
        if self.stopped {
            panic!("Stopped processor clock'ed!");
        }

        if self.cycles_ahead == 0 {
            let opcode = self.readb_pc(bus);
            self.curr_op = opcode;
            let instruction = Instruction::decode_op(opcode);
            info!("{:#06X}  {:02X} {}          A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} CYC:{}",
                self.regs.pc-1, opcode, instruction.operation,
                self.regs.a, self.regs.x, self.regs.y, self.regs.flags, self.regs.sp, self.cycles);
            self.cycles_ahead = self.run_instruction(bus, instruction);
        }
        self.cycles_ahead -= 1;
        self.cycles += 1
    }
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            regs: Registers::new(),
            curr_op: 0x00,
            cycles: 7,
            cycles_ahead: 0,
            stopped: false,
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
        return self.cycles_ahead > 0;
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

    // Pop a byte from the SP
    fn popb_sp<T: Memory>(&mut self, bus: &T) -> Byte {
        self.regs.sp += 1;
        let val = self.readb(bus, STACK_BASE_ADDR + self.regs.sp as Word);
        val
    }

    // Pop a word from the stack
    fn popw_sp<T: Memory>(&mut self, bus: &T) -> Word {
        let hi = self.popb_sp(bus);
        let lo = self.popb_sp(bus);
        (hi << 8) as Word & lo as Word
    }

    // Push a byte to the SP.
    fn pushb_sp<T: Memory>(&mut self, bus: &mut T, val: Byte) {
        self.writeb(bus, STACK_BASE_ADDR + self.regs.sp as Word, val);
        self.regs.sp -= 1;
    }

    // push a word to the stack, lo first, hi second
    fn pushw_sp<T: Memory>(&mut self, bus: &mut T, val: Word) {
        let lo = ((val >> 8) & LO) as Byte;
        let hi = (val & LO) as Byte;
        self.writeb(bus, STACK_BASE_ADDR + self.regs.sp as Word, lo);
        self.regs.sp -= 1;
        self.writeb(bus, STACK_BASE_ADDR + self.regs.sp as Word, hi);
        self.regs.sp -= 1;
    }

    // Set the flag with the corresponding mask
    fn set_flag(&mut self, flag: Byte, val: bool) {
        if val {
            self.regs.flags |= flag;
        } else {
            self.regs.flags &= !flag;
        } 
    }

    fn set_flag_nz(&mut self, val: Byte) {
        self.set_flag(ZERO, val == 0);
        self.set_flag(NEGATIVE, (val & NEGATIVE) > 0);
    }

    pub fn get_flag(&self, flag: Byte) -> Byte {
        if self.regs.flags & flag > 0 {
            1
        } else {
            0
        }
    }

    // Jump to address
    fn jump(&mut self, addr: Addr) {
        self.regs.pc = addr;
    }

    fn run_instruction<T: Memory>(&mut self, bus: &mut T, i: &Instruction) -> u8 {
        let (value, page_cross) = match &i.addr_mode {
            AddrMode::IMP => self.am_IMP(),
            AddrMode::IMM => self.am_IMM(),
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

        let extra_cycle_on_page_cross = match i.operation {
            Operation::ADC => self.op_ADC(bus, value),
            Operation::AND => self.op_AND(bus, value),
            Operation::ASL => self.op_ASL(bus, value),
            Operation::BCC => self.op_BCC(bus, value),
            Operation::BCS => self.op_BCS(value),
            Operation::BEQ => self.op_BEQ(bus, value),
            Operation::BIT => self.op_BIT(bus, value),
            Operation::BMI => self.op_BMI(bus, value),
            Operation::BNE => self.op_BNE(value),
            Operation::BPL => self.op_BPL(bus, value),
            Operation::BRK => self.op_BRK(bus),
            Operation::BVC => self.op_BVC(value),
            Operation::BVS => self.op_BVS(value),
            Operation::CLC => self.op_CLC(),
            Operation::CLD => self.op_CLD(),
            Operation::CLI => self.op_CLI(),
            Operation::CLV => self.op_CLV(),
            Operation::CMP => self.op_CMP(bus, value),
            Operation::CPX => self.op_CPX(bus, value),
            Operation::CPY => self.op_CPY(bus, value),
            Operation::DCP => self.op_DCP(bus, value),
            Operation::DEC => self.op_DEC(bus, value),
            Operation::DEX => self.op_DEX(),
            Operation::DEY => self.op_DEY(),
            Operation::EOR => self.op_EOR(bus, value),
            Operation::INC => self.op_INC(bus, value),
            Operation::INX => self.op_INX(),
            Operation::INY => self.op_INY(),
            Operation::ISB => self.op_ISB(bus, value),
            Operation::JMP => self.op_JMP(value),
            Operation::JSR => self.op_JSR(bus, value),
            Operation::KIL => self.op_KIL(),
            Operation::LAX => self.op_LAX(bus, value),
            Operation::LDA => self.op_LDA(bus, value),
            Operation::LDX => self.op_LDX(bus, value),
            Operation::LDY => self.op_LDY(bus, value),
            Operation::LSR => self.op_LSR(bus, value),
            Operation::NOP => self.op_NOP(),
            Operation::ORA => self.op_ORA(bus, value),
            Operation::PHA => self.op_PHA(bus),
            Operation::PHP => self.op_PHP(bus),
            Operation::PLA => self.op_PLA(bus),
            Operation::ROL => self.op_ROL(bus, value),
            Operation::PLP => self.op_PLP(bus),
            Operation::RLA => self.op_RLA(bus, value),
            Operation::ROR => self.op_ROR(bus, value),
            Operation::RRA => self.op_RRA(bus, value),
            Operation::RTI => self.op_RTI(bus),
            Operation::RTS => self.op_RTS(bus),
            Operation::SAX => self.op_SAX(bus, value),
            Operation::SBC => self.op_SBC(bus, value),
            Operation::SEC => self.op_SEC(),
            Operation::SED => self.op_SED(),
            Operation::SEI => self.op_SEI(),
            Operation::SLO => self.op_SLO(bus, value),
            Operation::SRE => self.op_SRE(bus, value),
            Operation::STA => self.op_STA(bus, value),
            Operation::STX => self.op_STX(bus, value),
            Operation::STY => self.op_STY(bus, value),
            Operation::TAX => self.op_TAX(),
            Operation::TAY => self.op_TAY(),
            Operation::TSX => self.op_TSX(),
            Operation::TXA => self.op_TXA(),
            Operation::TXS => self.op_TXS(),
            Operation::TYA => self.op_TYA(),
            _ => { 
                // OP not implemented. do nothing
                warn!("Operation not implemented: {:?}", i.operation);
                false
            }
        };

        if page_cross && extra_cycle_on_page_cross {
            i.cycles[0] + i.cycles[1]
        } else {
            i.cycles[0]
        }
    }

    // Implied aka no target
    fn am_IMP(&mut self) -> (Word, bool) {
       (0, false)
    }

    // Immediate, next byte of pc as addr for read (value is stores after
    // opcode)
    fn am_IMM(&mut self) -> (Word, bool) {
        let addr = self.regs.pc;
        self.regs.pc += 1;
        (addr, false)
    }

    // Absolute address on zero page
    fn am_ZP0<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let addr = self.readb_pc(bus);
        (LO & addr as Word, false) 
    }

    // Absolute address on zero page with x offset
    fn am_ZPX<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let addr = self.readb_pc(bus).wrapping_add(self.regs.x);
        (LO & addr as Word , false)
    }

    // Absolute address on zero page with y offset
    fn am_ZPY<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let addr = self.readb_pc(bus).wrapping_add(self.regs.y);
        (LO & addr as Word, false)
    }

    // Absolute address. Next 2 bytes of pc are the address
    fn am_ABS<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let addr = self.readw_pc(bus);
        (addr, false)
    }

    // Absolute address with offset. Next 2 bytes of pc are the address
    // additional cycle on page wrap
    fn am_ABX<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let tmp_addr = self.readw_pc(bus);
        let addr = tmp_addr.wrapping_add(self.regs.x as Word);

        let page_cross = addr & HI != tmp_addr & HI;
        (addr, page_cross)
    }

    // Absolute address with offset. Next 2 bytes of pc are the address
    // additional cycle on page wrap
    fn am_ABY<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let tmp_addr = self.readw_pc(bus);
        let addr = tmp_addr.wrapping_add(self.regs.y as Word);

        let page_cross = addr & HI != tmp_addr & HI;
        (addr, page_cross)
    }

    // Relative addressing. Only used for branching. The next byte on the 
    // pc is a signed offset from the current pc location 
    fn am_REL<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let rel_addr = self.readb_pc(bus) as Word;
        let base_addr = self.regs.pc;

        // If rel_addr > 0x8000, we substract 256 to make a negative jump
        let addr = if rel_addr < 128 {
            base_addr + rel_addr
        } else {
            base_addr + rel_addr - 256
        };
        (addr, false)
    }

    // the next 16 bits are an address. This address stores the real address
    // that is used for the operation.
    // Hardware bug: Normally, if lo of the supplied address is 0xFF, high byte
    // must be read from the next page. Instead it wraps around and reads from
    // the same page!
    fn am_IND<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let ind_addr = self.readw_pc(bus);
       
        // page boundary bug: If LO is 0x00FF, we are at the page border
        // and need to wrap around. So hi is fetched from 0x0000 instead of
        // 0x0100
        let addr = if ind_addr & LO == 0x00FF {
            let lo = self.readb(bus, ind_addr);
            let hi_addr = ind_addr - 0x00FF;
            let hi = self.readb(bus, hi_addr);
            (((hi as Word) << 8) | lo as Word)
        } else { // normal behaviour
            self.readw(bus, ind_addr)
        };

        (addr, false)
    }

    // the next 8 bits + x are an address on the zero page. This address stores the real address
    // that is used for the operation.
    fn am_IZX<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let ind_addr = self.readb_pc(bus);

        let lo_addr = ind_addr.wrapping_add(self.regs.x);
        let hi_addr = ind_addr.wrapping_add(self.regs.x).wrapping_add(1);
        let lo = self.readb(bus, lo_addr as Word);
        let hi = self.readb(bus, hi_addr as Word);
        ((hi as Word) << 8 | lo as Word, false) 
    }

    fn am_IZY<T: Memory>(&mut self, bus: &T) -> (Word, bool) {
        let ind_addr = self.readb_pc(bus);

        let lo = self.readb(bus, ind_addr as Word);
        let hi = self.readb(bus, ind_addr.wrapping_add(1) as Word);

        let addr = (hi as Word) << 8 | lo as Word;
        let addr = addr.wrapping_add(self.regs.y as Word);

        if addr & HI != (hi as Word) << 8 {
            (addr, true)
        } else {
            (addr, false)
        }
    }

    // Operations

    // ADC - Add with Carry
    // A,Z,C,N = A+M+C
    // This instruction adds the contents of a memory location to the
    // accumulator together with the carry bit. If overflow occurs the
    // carry bit is set, this enables multiple byte addition to be performed.
    // If the result is 0, Zero bit is set. If the result if negative,
    // Negative bit is set
    fn op_ADC<T: Memory>(&mut self, bus: &T, addr: Word) -> bool {
        let val = self.readb(bus, addr) as Word;
        let tmp = self.regs.a as Word + val + self.get_flag(CARRY) as Word;

        self.set_flag(CARRY, tmp > 255);
        self.set_flag_nz(tmp as Byte);
        
        // There are two cases where the overflow bit should be set. if we look
        // at the last bit of val and A: a) 0 + 0 = 1 b) 1 + 1 = 0. This
        // expression selects for both of them
        let is_overflown = (!(self.regs.a as Word ^ val) &
            (self.regs.a as Word ^ tmp)) & 0x0080 != 0;
        self.set_flag(OVERFLOW, is_overflown);
        
        self.regs.a = tmp as Byte; 
        true
    }

    // AND - Logical AND
    // A,Z,N = A&M
    // A logical AND is performed, bit by bit, on the accumulator contents
    // using the contents of a byte of memory.
    // If the result is 0, Zero bit is set. If the result if negative,
    // Negative bit is set
    fn op_AND<T: Memory>(&mut self, bus: &T, addr: Addr) -> bool {
        let val = self.readb(bus, addr);
        self.regs.a &= val;
        self.set_flag_nz(self.regs.a);
        true
    }

    // ASL - Arithmetic Shift Left
    // A,Z,C,N = M*2 or M,Z,C,N = M*2
    // This operation shifts all the bits of the accumulator or memory
    // contents one bit left. Bit 0 is set to 0 and bit 7 is placed in the
    // carry flag. The effect of this operation is to multiply the memory
    // contents by 2 (ignoring 2's complement considerations), setting the
    // carry if the result will not fit in 8 bits.
    // If the result is 0, Zero bit is set. If the result if negative,
    // Negative bit is set
    fn op_ASL<T: Memory>(&mut self, bus: &mut T, addr: Addr) -> bool {
        // LSR works on memory or A. We can differenciate by the addr mode
        let addr_mode = &Instruction::decode_op(self.curr_op).addr_mode;
        let val = if *addr_mode == AddrMode::IMP {
            self.regs.a
        } else {
            self.readb(bus, addr)
        };
        let shifted = (val << 1) as Byte;
        self.set_flag(CARRY, (val & 0b10000000) > 0);

        if *addr_mode == AddrMode::IMP {
            self.regs.a = shifted;
        } else {
            self.writeb(bus, addr, shifted);
        }

        self.set_flag_nz(shifted);
        false
    }


    // BCC - Branch if Carry Clear
    // If the carry flag is clear then add the relative displacement to
    // the program counter to cause a branch to a new location.
    fn op_BCC<T: Memory>(&mut self, bus: &T, addr: Addr) -> bool {
        if self.get_flag(CARRY) == 0 {
            self.jump(addr);
        }
        false
    }

        // BCC - Branch if Carry Set
    // If the carry flag is set then add the relative displacement to the
    // program counter to cause a branch to a new location.
    fn op_BCS(&mut self, addr: Addr) -> bool {
        if self.get_flag(CARRY) == 1 {
            self.jump(addr);
        }
        false
    }

    // BEQ - Branch if Equal
    // If the zero flag is set then add the relative displacement to
    // the program counter to cause a branch to a new location.
    fn op_BEQ<T: Memory>(&mut self, bus: &T, addr: Addr) -> bool {
        if self.get_flag(ZERO) == 1 {
            self.jump(addr);
        }
        false
    }

    // BIT - Bit Test
    // A & M, N = M7, V = M6
    // bits 7 and 6 of operand are transfered to bit 7 and 6 of SR (N,V);
    // the zeroflag is set to the result of operand AND accumulator.
    fn op_BIT<T: Memory>(&mut self, bus: &T, addr: Word) -> bool {
        let val = self.readb(bus, addr);
        self.set_flag(OVERFLOW, (val & OVERFLOW) > 1);
        self.set_flag(NEGATIVE, (val & NEGATIVE) > 1);
        self.set_flag(ZERO, (val & self.regs.a) == 0);
        false
    }

    // BMI - Branch if Minus
    // If the negative flag is set then add the relative displacement to the
    // program counter to cause a branch to a new location.
    fn op_BMI<T: Memory>(&mut self, bus: &T, addr: Addr) -> bool {
        if self.get_flag(NEGATIVE) == 1 {
            self.jump(addr);
        }
        false
    }

    // BNE - Branch if Not Equal
    // If the zero flag is clear then add the relative displacement to the
    // program counter to cause a branch to a new location.
    fn op_BNE(&mut self, addr: Addr) -> bool {
        if self.get_flag(ZERO) == 0 {
            self.jump(addr);
        }
        false
    }

    // BPL - Branch if Positive
    // If the negative flag is clear then add the relative displacement to
    // the program counter to cause a branch to a new location.
    fn op_BPL<T: Memory>(&mut self, bus: &T, addr: Addr) -> bool {
        if self.get_flag(NEGATIVE) == 0 {
            self.jump(addr);
        }
        false
    }

    // BRK - Force Interrupt
    // The BRK instruction forces the generation of an interrupt request.
    // The program counter and processor status are pushed on the stack
    // then the IRQ interrupt vector at $FFFE/F is loaded into the PC and
    // the break flag in the status set to one.
    fn op_BRK<T: Memory>(&mut self, bus: &mut T) -> bool {
        self.regs.pc += 1;
        self.set_flag(IRQ, true);

        // Push pc to stack
        self.pushb_sp(bus, (self.regs.pc >> 8) as Byte);
        self.pushb_sp(bus, self.regs.pc as Byte);
        
        // Push flags to stack
        self.set_flag(BREAK, true);
        self.pushb_sp(bus, self.regs.flags);
        self.set_flag(BREAK, false);

        // set PC to IRQ vector
        self.regs.pc = self.readw(bus, 0xFFFE);
        false 
    }

    // BVC - Branch if Overflow Clear
    // If the overflow flag is clear then add the relative displacement to
    // the program counter to cause a branch to a new location.
    fn op_BVC(&mut self, addr: Addr) -> bool {
        if self.get_flag(OVERFLOW) == 0 {
            self.jump(addr);
        }
        false
    }

    // BVS - Branch if Overflow Set
    // If the overflow flag is set then add the relative displacement to the
    // program counter to cause a branch to a new location.
    fn op_BVS(&mut self, addr: Addr) -> bool {
        if self.get_flag(OVERFLOW) == 1 {
            self.jump(addr);
        }
        false
    }

    // Clear carry flag
    fn op_CLC(&mut self) -> bool {
        self.set_flag(CARRY, false);
        false
    }
   
    // clear decimal flag
    fn op_CLD(&mut self) -> bool {
        self.set_flag(DECIMAL, false);
        false
    }


    // clear IRQ
    fn op_CLI(&mut self) -> bool {
        self.set_flag(IRQ, false);
        false
    }

    // clear Overflow
    fn op_CLV(&mut self) -> bool {
        self.set_flag(OVERFLOW, false);
        false
    }

    // CMP - Compare
    // Z,C,N = A-M
    // This instruction compares the contents of the accumulator with another
    // memory held value and sets the zero and carry flags as appropriate.
    fn op_CMP<T: Memory>(&mut self, bus: &mut T, addr: Addr) -> bool {
        let val = self.readb(bus, addr);
        let tmp = (self.regs.a as Word).wrapping_sub(val as Word);

        self.set_flag(CARRY, self.regs.a >= val);
        self.set_flag_nz(tmp as Byte);
        true
    }

    // Compare X
    fn op_CPX<T: Memory>(&mut self, bus: &T, addr: Addr) -> bool {
        let val = self.readb(bus, addr);
        let tmp = (self.regs.x as Word).wrapping_sub(val as Word);

        self.set_flag(CARRY, self.regs.x >= val);
        self.set_flag_nz(tmp as Byte);
        true
    }

    // Compare Y
    fn op_CPY<T: Memory>(&mut self, bus: &T, addr: Addr) -> bool {
        let val = self.readb(bus, addr);
        let tmp = (self.regs.y as Word).wrapping_sub(val as Word);

        self.set_flag(CARRY, self.regs.y >= val);
        self.set_flag_nz(tmp as Byte);
        true
    }

    // Unofficial: DEC value, then CMP 
    fn op_DCP<T: Memory>(&mut self, bus: &mut T, addr: Word) -> bool {
        self.op_DEC(bus, addr);
        self.op_CMP(bus, addr);
        false
    }

    // DEC - Decrement Memory
    // M,Z,N = M-1
    // Subtracts one from the value held at a specified memory location
    // setting the zero and negative flags as appropriate.
    fn op_DEC<T: Memory>(&mut self, bus: &mut T, addr: Word) -> bool {
        let val = self.readb(bus, addr);
        let val = val.wrapping_sub(1);
        self.writeb(bus, addr, val);

        self.set_flag_nz(val);
        false
    }

    // DEX - Decrement X Register
    // X,Z,N = X-1
    // Subtracts one from the X register setting the zero and negative
    // flags as appropriate.
    fn op_DEX(&mut self) -> bool {
        self.regs.x = self.regs.x.wrapping_sub(1);

        self.set_flag_nz(self.regs.x);
        false
    }

    // DEY - Decrement X Register
    // X,Z,N = Y-1
    // Subtracts one from the Y register setting the zero and negative
    // flags as appropriate.
    fn op_DEY(&mut self) -> bool {
        self.regs.y = self.regs.y.wrapping_sub(1);

        self.set_flag_nz(self.regs.y);
        false
    }

    // EOR - Exclusive OR
    // A,Z,N = A^M
    // An exclusive OR is performed, bit by bit, on the accumulator contents
    // using the contents of a byte of memory.
    fn op_EOR<T: Memory>(&mut self, bus: &T, addr: Addr) -> bool {
        let val = self.readb(bus, addr);
        self.regs.a = self.regs.a ^ val;

        self.set_flag_nz(self.regs.a);
        true
    }

    // INC - Increment Memory
    // M,Z,N = M+1
    // Adds one to the value held at a specified memory location setting the
    // zero and negative flags as appropriate.
    fn op_INC<T: Memory>(&mut self, bus: &mut T, addr: Word) -> bool {
        let val = self.readb(bus, addr);
        let val = val.wrapping_add(1);
        self.writeb(bus, addr, val);
        self.set_flag_nz(val);
        false
    }

    // INX - Increment X Register
    // X,Z,N = X+1
    // Adds one to the X register setting the zero and negative flags
    // as appropriate.
    fn op_INX(&mut self) -> bool {
        self.regs.x = self.regs.x.wrapping_add(1);
        self.set_flag_nz(self.regs.x);
        false
    }

    // INY - Increment Y Register
    // Y,Z,N = Y+1
    // Adds one to the Y register setting the zero and negative flags as appropriate.
    fn op_INY(&mut self) -> bool {
        self.regs.y = self.regs.y.wrapping_add(1);
        self.set_flag_nz(self.regs.y);
        false
    }

    // Unofficial opcode: INC, then SBC
    fn op_ISB<T: Memory>(&mut self, bus: &mut T, addr: Addr) -> bool {
        self.op_INC(bus, addr);
        self.op_SBC(bus, addr);
        false
    }

    // Jump to address (set pc)
    fn op_JMP(&mut self, addr: Addr) -> bool {
        self.jump(addr);
        false
    }

    // Jump to subroutine (leaves trace on the stack)
    fn op_JSR<T: Memory>(&mut self, bus: &mut T, addr: Word) -> bool {
        self.regs.pc -= 1;
        self.writeb(bus, STACK_BASE_ADDR + self.regs.sp as Word, ((self.regs.pc >> 8) & 0x00ff) as Byte);
        self.regs.sp -= 1;
        self.writeb(bus, STACK_BASE_ADDR + self.regs.sp as Word, (self.regs.pc & 0x00ff) as Byte);
        self.regs.sp -= 1;
        self.jump(addr);
        false
    }

    // Unofficial: KIL halts the processor
    fn op_KIL(&mut self) -> bool {
        self.stopped = true;
        info!("Processor received KIL instruction.");

        false
    }

    // Unofficial op code! Shortcut for LDA, TAX
    fn op_LAX<T: Memory>(&mut self, bus: &T, addr: Word) -> bool {
        self.op_LDA(bus, addr);
        self.op_TAX();
        false
    }

    // Read value from addr into A
    fn op_LDA<T: Memory>(&mut self, bus: &T, addr: Word) -> bool {
        let val = self.readb(bus, addr);
        self.regs.a = val;
        self.set_flag_nz(val);
        true
    }

    // Read value from addr into X
    fn op_LDX<T: Memory>(&mut self, bus: &T, addr: Word) -> bool {
        let val = self.readb(bus, addr);
        self.regs.x = val;
        self.set_flag_nz(val);
        true
    }

    // Read value from addr into Y
    fn op_LDY<T: Memory>(&mut self, bus: &T, addr: Word) -> bool {
        let val = self.readb(bus, addr);
        self.regs.y = val;
        self.set_flag_nz(val);
        true
    }

    // LSR - Logical Shift Right
    // A,C,Z,N = A/2 or M,C,Z,N = M/2
    // Each of the bits in A or M is shift one place to the right. The bit
    // that was in bit 0 is shifted into the carry flag. Bit 7 is set to zero.
    fn op_LSR<T: Memory>(&mut self, bus: &mut T, addr: Addr) -> bool {
        // LSR works on memory or A. We can differenciate by the addr mode
        let addr_mode = &Instruction::decode_op(self.curr_op).addr_mode;
        let val = if *addr_mode == AddrMode::IMP {
            self.regs.a as Word
        } else {
            self.readb(bus, addr) as Word
        };

        self.set_flag(CARRY, (val & 0b00000001) != 0);
        let shifted = (val >> 1) as Byte;
        self.set_flag_nz(shifted);

        if *addr_mode == AddrMode::IMP {
            self.regs.a = shifted;
        } else {
            self.writeb(bus, addr, shifted);
        }
        false
    }

    // does nothing
    fn op_NOP(&mut self) -> bool {
        false
    }

    // ORA - Logical Inclusive OR
    // A,Z,N = A|M
    // An inclusive OR is performed, bit by bit, on the accumulator contents
    // using the contents of a byte of memory.
    fn op_ORA<T: Memory>(&mut self, bus: &T, addr: Addr) -> bool {
        self.regs.a = self.regs.a | self.readb(bus, addr);
        self.set_flag_nz(self.regs.a);
        true
    }

    // PHA - Push Accumulator
    // Pushes a copy of the accumulator on to the stack.
    fn op_PHA<T: Memory>(&mut self, bus: &mut T) -> bool {
        self.pushb_sp(bus, self.regs.a);
        false
    }

    // PHP - Push Processor Status
    // Pushes a copy of the status flags on to the stack.
    fn op_PHP<T: Memory>(&mut self, bus: &mut T) -> bool {
        let tmp = self.regs.flags | BREAK;
        self.pushb_sp(bus, tmp);
        self.set_flag(BREAK, false);
        false
    }

    // Read from stack into A
    fn op_PLA<T: Memory>(&mut self, bus: &T) -> bool {
        self.regs.a = self.popb_sp(bus);
        self.set_flag_nz(self.regs.a);
        false
    }

    // PLP - Pull Processor Status
    // Pulls an 8 bit value from the stack and into the processor flags. The
    // flags will take on new states as determined by the value pulled.
    fn op_PLP<T: Memory>(&mut self, bus: &T) -> bool {
        self.regs.flags = self.popb_sp(bus);

        // Im not sure why this is set to false and stack value is not used
        // but that's how the nestest.log shows it..
        self.set_flag(BREAK, false);  
        false
    }

    // ROL - Rotate Left
    // Move each of the bits in either A or M one place to the left. Bit 0 is
    // filled with the current value of the carry flag whilst the old bit 7
    // becomes the new carry flag value.
    fn op_ROL<T: Memory>(&mut self, bus: &mut T, addr: Addr) -> bool {
        let addr_mode = &Instruction::decode_op(self.curr_op).addr_mode;
        let val = if *addr_mode == AddrMode::IMP {
            self.regs.a as Word
        } else {
            self.readb(bus, addr) as Word
        };
        let shifted = (val << 1) as Byte | self.get_flag(CARRY);
        
        self.set_flag(CARRY, (val & 0b10000000) > 0);
        self.set_flag_nz(shifted);

        if *addr_mode == AddrMode::IMP {
            self.regs.a = shifted as Byte;
        } else {
            self.writeb(bus, addr, shifted);
        }
        false
    }

    // Unofficial: ROL and then AND
    fn op_RLA<T: Memory>(&mut self, bus: &mut T, addr: Addr) -> bool {
        self.op_ROL(bus, addr);
        self.op_AND(bus, addr);

        false
    }

    // Unofficial: Performs ROR + ADC
    fn op_RRA<T: Memory>(&mut self, bus: &mut T, addr: Addr) -> bool {
        self.op_ROR(bus, addr);
        self.op_ADC(bus, addr);
        false
    }

    // ROR - Rotate Right
    // Move each of the bits in either A or M one place to the right. Bit 7 is
    // filled with the current value of the carry flag whilst the old bit 0
    // becomes the new carry flag value.
    fn op_ROR<T: Memory>(&mut self, bus: &mut T, addr: Addr) -> bool {
        let addr_mode = &Instruction::decode_op(self.curr_op).addr_mode;
        let val = if *addr_mode == AddrMode::IMP {
            self.regs.a as Word
        } else {
            self.readb(bus, addr) as Word
        };

        let shifted = (val >> 1) as Byte | (self.get_flag(CARRY) << 7);
        
        self.set_flag(CARRY, (val & 0b00000001) > 0);
        self.set_flag_nz(shifted);

        if *addr_mode == AddrMode::IMP {
            self.regs.a = shifted;
        } else {
            self.writeb(bus, addr, shifted);
        }
        false
    }

    // RTI - Return from Interrupt
    // The RTI instruction is used at the end of an interrupt processing
    // routine. It pulls the processor flags from the stack followed by the
    // program counter.
    fn op_RTI<T: Memory>(&mut self, bus: &T) -> bool {
        self.regs.flags = self.popb_sp(bus);
        self.regs.flags &= !BREAK;
        
        let pc_lo = self.popb_sp(bus) as Word;
        let pc_hi = self.popb_sp(bus) as Word;
        self.regs.pc = pc_hi << 8 | pc_lo;
        false
    }

    // RTS - Return from Subroutine
    // The RTS instruction is used at the end of a subroutine to return to the
    // calling routine. It pulls the program counter (minus one) from the stack.
    fn op_RTS<T: Memory>(&mut self, bus: &T) -> bool {
        self.regs.sp += 1;
        let lo = self.readb(bus, 0x0100 + self.regs.sp as Addr);
        self.regs.sp += 1;
        let hi = self.readb(bus, 0x0100 + self.regs.sp as Addr);
        let addr = (hi as Addr) << 8 | lo as Addr;
        self.regs.pc = addr + 1; 
        false
    }

    // Unofficial: Stores bitwise AND of A and X
    fn op_SAX<T: Memory>(&mut self, bus: &mut T, addr: Addr) -> bool {
        let val = self.regs.a & self.regs.x;
        self.writeb(bus, addr, val); 
        false
    }


    // SBC - Subtract with Carry
    // A,Z,C,N = A-M-(1-C)
    // This instruction subtracts the contents of a memory location to the
    // accumulator together with the not of the carry bit. If overflow occurs
    // the carry bit is clear, this enables multiple byte subtraction to be
    // performed.
    fn op_SBC<T: Memory>(&mut self, bus: &T, addr: Addr) -> bool {
        let val = self.readb(bus, addr) as Word;
        
        // invert buttom 8 bits
        let val = val ^ LO;

        // Now it's similar to ADC
        let tmp = self.regs.a as Word + val + self.get_flag(CARRY) as Word;

        self.set_flag(CARRY, tmp > 255);
        self.set_flag_nz(tmp as Byte);
        
        // There are two cases where the overflow bit should be set. if we look
        // at the last bit of val and A: a) 0 + 0 = 1 b) 1 + 1 = 0. This
        // expression selects for both of them
        let is_overflown = (!(self.regs.a as Word ^ val) &
            (self.regs.a as Word ^ tmp)) & 0x0080 != 0;
        self.set_flag(OVERFLOW, is_overflown);
        
        self.regs.a = tmp as Byte; 
        true
    }

    // set carry
    fn op_SEC(&mut self) -> bool {
        self.set_flag(CARRY, true);
        false
    }

    //  SED - Set Decimal Flag
    // D = 1
    // Set the decimal mode flag to one.
    fn op_SED(&mut self) -> bool {
        self.set_flag(DECIMAL, true);
        false
    }

    // set irq flag
    fn op_SEI(&mut self) -> bool {
        self.set_flag(IRQ, true);
        false
    }

    // Unofficial: ASL + ORA
    fn op_SLO<T: Memory>(&mut self, bus: &mut T, addr: Word) -> bool {
        self.op_ASL(bus, addr);
        self.op_ORA(bus, addr);
        false
    }

    // Unofficial: LSR + EOR
    fn op_SRE<T: Memory>(&mut self, bus: &mut T, addr: Word) -> bool {
        self.op_LSR(bus, addr);
        self.op_EOR(bus, addr);
        false
    }

    // Push A reg to memory
    fn op_STA<T: Memory>(&mut self, bus: &mut T, addr: Word) -> bool {
        self.writeb(bus, addr, self.regs.a);
        false
    }

    // Push X reg to memory
    fn op_STX<T: Memory>(&mut self, bus: &mut T, addr: Word) -> bool {
        self.writeb(bus, addr, self.regs.x);
        false
    }

    // Push Y reg to memory
    fn op_STY<T: Memory>(&mut self, bus: &mut T, addr: Word) -> bool {
        self.writeb(bus, addr, self.regs.y);
        false
    }

    // a to x
    fn op_TAX(&mut self) -> bool {
        self.regs.x = self.regs.a;
        self.set_flag_nz(self.regs.x);
        false
    }

    // a to y
    fn op_TAY(&mut self) -> bool {
        self.regs.y = self.regs.a;
        self.set_flag_nz(self.regs.y);
        false
    }

    // stack pointer to x
    fn op_TSX(&mut self) -> bool {
        self.regs.x = self.regs.sp;
        self.set_flag_nz(self.regs.x);
        false
    }

    // transfer x to a
    fn op_TXA(&mut self) -> bool {
        self.regs.a = self.regs.x;
        self.set_flag_nz(self.regs.x);
        false
    }

    // transfer y to a
    fn op_TYA(&mut self) -> bool {
        self.regs.a = self.regs.y;
        self.set_flag_nz(self.regs.a);
        false
    }   

    // transfer x to stack
    fn op_TXS(&mut self) -> bool {
        self.regs.sp = self.regs.x;
        false
    } 
}

impl Debug for CPU {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:?}, op: {:x}, cycle: {:?}",
            self.regs, self.curr_op, self.cycles)
    } 
}


