#![no_std]

pub struct CentralProcessingUnit {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub sp: u8,
    pub pc: u16,
    pub status: u8,
    pub memory: [u8; 0x10000],
}

// Initialization
#[allow(clippy::new_without_default)]
impl CentralProcessingUnit {
    pub const fn new() -> Self {
        static ZEROED_MEMORY: [u8; 0x10000] = [0; 0x10000];

        Self {
            a: 0,
            x: 0,
            y: 0,
            sp: 0xFD,
            pc: 0,
            status: Self::STATUS_FLAG_U | Self::STATUS_FLAG_I,
            memory: ZEROED_MEMORY,
        }
    }

    pub const fn new_with_memory(memory: [u8; 0x10000]) -> Self {
        Self {
            a: 0,
            x: 0,
            y: 0,
            sp: 0xFD,
            pc: ((memory[0xFFFD] as u16) << 8) | (memory[0xFFFC] as u16),
            status: Self::STATUS_FLAG_U | Self::STATUS_FLAG_I,
            memory,
        }
    }

    pub const fn reset(&mut self) {
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sp = 0xFD;
        self.pc = self.read_word(0xFFFC);
        self.status = Self::STATUS_FLAG_U | Self::STATUS_FLAG_I;
    }
}

// Memory & Stack
impl CentralProcessingUnit {
    #[inline(always)]
    const fn read_byte(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    #[inline(always)]
    const fn write_byte(&mut self, addr: u16, value: u8) {
        self.memory[addr as usize] = value;
    }

    #[inline(always)]
    const fn read_word(&self, addr: u16) -> u16 {
        ((self.read_byte(addr.wrapping_add(1)) as u16) << 8) | (self.read_byte(addr) as u16)
    }

    #[inline(always)]
    const fn push(&mut self, value: u8) {
        self.write_byte(0x0100 + self.sp as u16, value);
        self.sp = self.sp.wrapping_sub(1);
    }

    #[inline(always)]
    const fn pop(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        self.read_byte(0x0100 + self.sp as u16)
    }
}

// Status Flags
impl CentralProcessingUnit {
    const STATUS_FLAG_C: u8 = 1 << 0;
    const STATUS_FLAG_Z: u8 = 1 << 1;
    const STATUS_FLAG_I: u8 = 1 << 2;
    const STATUS_FLAG_D: u8 = 1 << 3;
    const STATUS_FLAG_B: u8 = 1 << 4;
    const STATUS_FLAG_U: u8 = 1 << 5;
    const STATUS_FLAG_V: u8 = 1 << 6;
    const STATUS_FLAG_N: u8 = 1 << 7;

    #[inline(always)]
    const fn set_flag(&mut self, flag: u8, state: bool) {
        self.status ^= (self.status ^ (-(state as i8) as u8)) & flag;
    }

    #[inline(always)]
    const fn get_flag(&self, flag: u8) -> bool {
        self.status & flag != 0
    }

    #[inline(always)]
    const fn update_zn(&mut self, result: u8) {
        self.status ^= (self.status
            ^ ((-((result == 0) as i8) as u8) & Self::STATUS_FLAG_Z
                | (-(((result & 0x80) != 0) as i8) as u8) & Self::STATUS_FLAG_N))
            & (Self::STATUS_FLAG_Z | Self::STATUS_FLAG_N);
    }
}

// Addressing Modes
impl CentralProcessingUnit {
    const fn immediate(&mut self) -> u16 {
        let addr = self.pc;
        self.pc = self.pc.wrapping_add(1);

        addr
    }

    const fn zeropage(&mut self) -> u16 {
        let addr = self.read_byte(self.pc) as u16;
        self.pc = self.pc.wrapping_add(1);

        addr
    }

    const fn zeropage_x(&mut self) -> u16 {
        let addr = self.read_byte(self.pc);
        self.pc = self.pc.wrapping_add(1);

        addr.wrapping_add(self.x) as u16
    }

    const fn zeropage_y(&mut self) -> u16 {
        let addr = self.read_byte(self.pc);
        self.pc = self.pc.wrapping_add(1);

        addr.wrapping_add(self.y) as u16
    }

    const fn absolute(&mut self) -> u16 {
        let addr = self.read_word(self.pc);
        self.pc = self.pc.wrapping_add(2);

        addr
    }

    const fn absolute_x(&mut self) -> u16 {
        let addr = self.read_word(self.pc);
        self.pc = self.pc.wrapping_add(2);

        addr.wrapping_add(self.x as u16)
    }

    const fn absolute_y(&mut self) -> u16 {
        let addr = self.read_word(self.pc);
        self.pc = self.pc.wrapping_add(2);

        addr.wrapping_add(self.y as u16)
    }

    const fn indirect(&mut self) -> u16 {
        let addr = self.read_word(self.pc);
        self.pc = self.pc.wrapping_add(2);

        // I believe this is the correct behavior for the 6502 hardware bug.
        (self.read_byte((addr & 0xFF00) | ((addr.wrapping_add(1)) & 0xFF)) as u16) << 8
            | (self.read_byte(addr) as u16)
    }

    const fn indexed_indirect(&mut self) -> u16 {
        let addr = self.read_byte(self.pc).wrapping_add(self.x) as u16;
        self.pc = self.pc.wrapping_add(1);

        ((self.read_byte(addr.wrapping_add(1)) as u16) << 8) | (self.read_byte(addr) as u16)
    }

    const fn indirect_indexed(&mut self) -> u16 {
        let addr = self.read_byte(self.pc) as u16;
        self.pc = self.pc.wrapping_add(1);

        (((self.read_byte(addr.wrapping_add(1)) as u16) << 8) | (self.read_byte(addr) as u16))
            .wrapping_add(self.y as u16)
    }
}

// Arithmetic & Logic Instructions
impl CentralProcessingUnit {
    const fn adc(&mut self, value: u8) {
        let sum = self.a as u16 + value as u16 + (self.status & Self::STATUS_FLAG_C) as u16;
        let mut result = sum as u8;

        if self.get_flag(Self::STATUS_FLAG_D) {
            result =
                result.wrapping_add(((result & 0x0F) > 9) as u8 * 0x06 + (sum > 0x99) as u8 * 0x60);

            self.set_flag(Self::STATUS_FLAG_C, sum > 0x99);
            self.set_flag(Self::STATUS_FLAG_V, false);
        } else {
            self.set_flag(Self::STATUS_FLAG_C, sum > 0xFF);
            self.set_flag(
                Self::STATUS_FLAG_V,
                (!(self.a ^ value) & (self.a ^ result) & 0x80) != 0,
            );
        }

        self.a = result;
        self.update_zn(result);
    }

    const fn sbc(&mut self, value: u8) {
        let original_carry = self.get_flag(Self::STATUS_FLAG_C);
        self.set_flag(Self::STATUS_FLAG_C, true);
        self.adc(value ^ 0xFF);
        if !original_carry {
            let (result, _) = self.a.overflowing_sub(1);
            self.a = result;
            self.update_zn(result);
        }
    }

    const fn ora(&mut self, value: u8) {
        self.a |= value;
        self.update_zn(self.a);
    }

    const fn and(&mut self, value: u8) {
        self.a &= value;
        self.update_zn(self.a);
    }

    const fn eor(&mut self, value: u8) {
        self.a ^= value;
        self.update_zn(self.a);
    }

    const fn cmp(&mut self, reg: u8, value: u8) {
        let result = reg.wrapping_sub(value);
        self.set_flag(Self::STATUS_FLAG_C, reg >= value);
        self.update_zn(result);
    }

    const fn bit(&mut self, value: u8) {
        self.set_flag(Self::STATUS_FLAG_Z, (self.a & value) == 0);
        self.set_flag(Self::STATUS_FLAG_N, value & 0x80 != 0);
        self.set_flag(Self::STATUS_FLAG_V, value & 0x40 != 0);
    }

    const fn inc(&mut self, addr: u16) {
        let result = self.read_byte(addr).wrapping_add(1);
        self.write_byte(addr, result);
        self.update_zn(result);
    }

    const fn dec(&mut self, addr: u16) {
        let result = self.read_byte(addr).wrapping_sub(1);
        self.write_byte(addr, result);
        self.update_zn(result);
    }
}

// Shifts & Rotates
impl CentralProcessingUnit {
    const fn asl_acc(&mut self) {
        self.set_flag(Self::STATUS_FLAG_C, self.a & 0x80 != 0);
        self.a <<= 1;
        self.update_zn(self.a);
    }

    const fn asl_mem(&mut self, addr: u16) {
        let value = self.read_byte(addr);
        self.set_flag(Self::STATUS_FLAG_C, value & 0x80 != 0);
        let result = value << 1;
        self.write_byte(addr, result);
        self.update_zn(result);
    }

    const fn lsr_acc(&mut self) {
        self.set_flag(Self::STATUS_FLAG_C, self.a & 0x01 != 0);
        self.a >>= 1;
        self.update_zn(self.a);
    }

    const fn lsr_mem(&mut self, addr: u16) {
        let value = self.read_byte(addr);
        self.set_flag(Self::STATUS_FLAG_C, value & 0x01 != 0);
        let result = value >> 1;
        self.write_byte(addr, result);
        self.update_zn(result);
    }

    const fn rol_acc(&mut self) {
        let old = self.a;
        let new_carry = old & 0x80 != 0;
        self.a = (old << 1)
            | if self.get_flag(Self::STATUS_FLAG_C) {
                1
            } else {
                0
            };
        self.set_flag(Self::STATUS_FLAG_C, new_carry);
        self.update_zn(self.a);
    }

    const fn rol_mem(&mut self, addr: u16) {
        let value = self.read_byte(addr);
        let new_carry = value & 0x80 != 0;
        let result = (value << 1)
            | if self.get_flag(Self::STATUS_FLAG_C) {
                1
            } else {
                0
            };
        self.write_byte(addr, result);
        self.set_flag(Self::STATUS_FLAG_C, new_carry);
        self.update_zn(result);
    }

    const fn ror_acc(&mut self) {
        let old = self.a;
        let old_carry = if self.get_flag(Self::STATUS_FLAG_C) {
            0x80
        } else {
            0
        };
        let new_carry = old & 0x01 != 0;
        let result = (old >> 1) | old_carry;
        self.a = result;
        self.set_flag(Self::STATUS_FLAG_C, new_carry);
        self.update_zn(result);
    }

    const fn ror_mem(&mut self, addr: u16) {
        let value = self.read_byte(addr);
        let old_carry = if self.get_flag(Self::STATUS_FLAG_C) {
            0x80
        } else {
            0
        };
        let new_carry = value & 0x01 != 0;
        let result = (value >> 1) | old_carry;
        self.write_byte(addr, result);
        self.set_flag(Self::STATUS_FLAG_C, new_carry);
        self.update_zn(result);
    }
}

// Control Flow
impl CentralProcessingUnit {
    const fn branch(&mut self, condition: bool) {
        let offset = self.read_byte(self.pc) as i8;
        self.pc = self.pc.wrapping_add(1);
        if condition {
            self.pc = self.pc.wrapping_add(offset as u16);
        }
    }
}

// Execution
impl CentralProcessingUnit {
    /// Execute one 6502 instruction
    ///
    /// # Supported instructions
    /// - **System:**        `BRK`, `RTI`, `RTS`  
    /// - **Jump/Call:**     `JSR`, `JMP` (absolute & indirect)  
    /// - **Branches:**      `BPL`, `BMI`, `BVC`, `BVS`, `BCC`, `BCS`, `BNE`, `BEQ`  
    /// - **Flag ops:**      `CLC`, `SEC`, `CLI`, `SEI`, `CLV`, `CLD`, `SED`  
    /// - **Logical:**       `ORA`, `AND`, `EOR`, `BIT`  
    /// - **Arithmetic:**    `ADC`, `SBC`, `INC`, `INX`, `INY`, `DEC`, `DEX`, `DEY`  
    /// - **Compare:**       `CMP`, `CPX`, `CPY`  
    /// - **Data xfer:**     `LDA`, `LDX`, `LDY`, `STA`, `STX`, `STY`  
    /// - **Reg xfer:**      `TAX`, `TAY`, `TXA`, `TYA`, `TSX`, `TXS`  
    /// - **Stack ops:**     `PHA`, `PLA`, `PHP`, `PLP`  
    /// - **Shifts/Rots:**   `ASL`, `LSR`, `ROL`, `ROR` (acc & mem)  
    /// - **Misc:**          `NOP`  
    ///
    /// # Illegal Opcodes
    /// Currently all illegal opcodes are treated as `NOP` (no operation).
    #[inline(always)]
    pub const fn step(&mut self) {
        let opcode = self.read_byte(self.pc);
        self.pc = self.pc.wrapping_add(1);

        //TODO: sort into order as its currently by instr type
        match opcode {
            // BRK
            0x00 => {
                self.pc = self.pc.wrapping_add(1);
                self.push((self.pc >> 8) as u8);
                self.push((self.pc & 0xFF) as u8);
                self.set_flag(Self::STATUS_FLAG_B, true);
                self.push(self.status);
                self.set_flag(Self::STATUS_FLAG_I, true);
                self.pc = self.read_word(0xFFFE);
            }

            // RTI
            0x40 => {
                self.status = self.pop();
                self.status &= !(Self::STATUS_FLAG_B | Self::STATUS_FLAG_I);
                self.status |= Self::STATUS_FLAG_U;
                let lo = self.pop() as u16;
                let hi = self.pop() as u16;
                self.pc = (hi << 8) | lo;
            }

            // RTS
            0x60 => {
                let lo = self.pop() as u16;
                let hi = self.pop() as u16;
                self.pc = ((hi << 8) | lo).wrapping_add(1);
            }

            // JSR
            0x20 => {
                let addr = self.read_word(self.pc);
                self.pc = self.pc.wrapping_add(2);
                let ret = self.pc.wrapping_sub(1);
                self.push((ret >> 8) as u8);
                self.push((ret & 0xFF) as u8);
                self.pc = addr;
            }

            // JMP abs
            0x4C => self.pc = self.absolute(),

            // JMP ind
            0x6C => self.pc = self.indirect(),

            // BPL
            0x10 => self.branch(!self.get_flag(Self::STATUS_FLAG_N)),

            // BMI
            0x30 => self.branch(self.get_flag(Self::STATUS_FLAG_N)),

            // BVC
            0x50 => self.branch(!self.get_flag(Self::STATUS_FLAG_V)),

            // BVS
            0x70 => self.branch(self.get_flag(Self::STATUS_FLAG_V)),

            // BCC
            0x90 => self.branch(!self.get_flag(Self::STATUS_FLAG_C)),

            // BCS
            0xB0 => self.branch(self.get_flag(Self::STATUS_FLAG_C)),

            // BNE
            0xD0 => self.branch(!self.get_flag(Self::STATUS_FLAG_Z)),

            // BEQ
            0xF0 => self.branch(self.get_flag(Self::STATUS_FLAG_Z)),

            // CLC
            0x18 => self.set_flag(Self::STATUS_FLAG_C, false),

            // SEC
            0x38 => self.set_flag(Self::STATUS_FLAG_C, true),

            // CLI
            0x58 => self.set_flag(Self::STATUS_FLAG_I, false),

            // SEI
            0x78 => self.set_flag(Self::STATUS_FLAG_I, true),

            // CLV
            0xB8 => self.set_flag(Self::STATUS_FLAG_V, false),

            // CLD
            0xD8 => self.set_flag(Self::STATUS_FLAG_D, false),

            // SED
            0xF8 => self.set_flag(Self::STATUS_FLAG_D, true),

            // ORA #imm
            0x09 => {
                let addr = self.immediate();
                let v = self.read_byte(addr);

                self.ora(v)
            }

            // ORA zp
            0x05 => {
                let addr = self.zeropage();
                let v = self.read_byte(addr);

                self.ora(v)
            }

            // ORA zp,X
            0x15 => {
                let addr = self.zeropage_x();
                let v = self.read_byte(addr);

                self.ora(v)
            }

            // ORA abs
            0x0D => {
                let addr = self.absolute();
                let v = self.read_byte(addr);

                self.ora(v)
            }

            // ORA abs,X
            0x1D => {
                let addr = self.absolute_x();
                let v = self.read_byte(addr);

                self.ora(v)
            }

            // ORA abs,Y
            0x19 => {
                let addr = self.absolute_y();
                let v = self.read_byte(addr);

                self.ora(v)
            }

            // ORA (zp,X)
            0x01 => {
                let addr = self.indexed_indirect();
                let v = self.read_byte(addr);
                self.ora(v)
            }

            // ORA (zp),Y
            0x11 => {
                let addr = self.indirect_indexed();
                let v = self.read_byte(addr);
                self.ora(v)
            }

            // AND #imm
            0x29 => {
                let addr = self.immediate();
                let v = self.read_byte(addr);
                self.and(v)
            }

            // AND zp
            0x25 => {
                let addr = self.zeropage();
                let v = self.read_byte(addr);
                self.and(v)
            }

            // AND zp,X
            0x35 => {
                let addr = self.zeropage_x();
                let v = self.read_byte(addr);
                self.and(v)
            }

            // AND abs
            0x2D => {
                let addr = self.absolute();
                let v = self.read_byte(addr);
                self.and(v)
            }

            // AND abs,X
            0x3D => {
                let addr = self.absolute_x();
                let v = self.read_byte(addr);
                self.and(v)
            }

            // AND abs,Y
            0x39 => {
                let addr = self.absolute_y();
                let v = self.read_byte(addr);
                self.and(v)
            }

            // AND (zp,X)
            0x21 => {
                let addr = self.indexed_indirect();
                let v = self.read_byte(addr);
                self.and(v)
            }

            // AND (zp),Y
            0x31 => {
                let addr = self.indirect_indexed();
                let v = self.read_byte(addr);
                self.and(v)
            }

            // EOR #imm
            0x49 => {
                let addr = self.immediate();
                let v = self.read_byte(addr);
                self.eor(v)
            }

            // EOR zp
            0x45 => {
                let addr = self.zeropage();
                let v = self.read_byte(addr);
                self.eor(v)
            }

            // EOR zp,X
            0x55 => {
                let addr = self.zeropage_x();
                let v = self.read_byte(addr);
                self.eor(v)
            }

            // EOR abs
            0x4D => {
                let addr = self.absolute();
                let v = self.read_byte(addr);
                self.eor(v)
            }

            // EOR abs,X
            0x5D => {
                let addr = self.absolute_x();
                let v = self.read_byte(addr);
                self.eor(v)
            }

            // EOR abs,Y
            0x59 => {
                let addr = self.absolute_y();
                let v = self.read_byte(addr);
                self.eor(v)
            }

            // EOR (zp,X)
            0x41 => {
                let addr = self.indexed_indirect();
                let v = self.read_byte(addr);
                self.eor(v)
            }

            // EOR (zp),Y
            0x51 => {
                let addr = self.indirect_indexed();
                let v = self.read_byte(addr);
                self.eor(v)
            }

            // BIT zp
            0x24 => {
                let addr = self.zeropage();
                let v = self.read_byte(addr);
                self.bit(v)
            }

            // BIT abs
            0x2C => {
                let addr = self.absolute();
                let v = self.read_byte(addr);
                self.bit(v)
            }

            // ADC #imm
            0x69 => {
                let addr = self.immediate();
                let v = self.read_byte(addr);
                self.adc(v)
            }

            // ADC zp
            0x65 => {
                let addr = self.zeropage();
                let v = self.read_byte(addr);
                self.adc(v)
            }

            // ADC zp,X
            0x75 => {
                let addr = self.zeropage_x();
                let v = self.read_byte(addr);
                self.adc(v)
            }

            // ADC abs
            0x6D => {
                let addr = self.absolute();
                let v = self.read_byte(addr);
                self.adc(v)
            }

            // ADC abs,X
            0x7D => {
                let addr = self.absolute_x();
                let v = self.read_byte(addr);
                self.adc(v)
            }

            // ADC abs,Y
            0x79 => {
                let addr = self.absolute_y();
                let v = self.read_byte(addr);
                self.adc(v)
            }

            // ADC (zp,X)
            0x61 => {
                let addr = self.indexed_indirect();
                let v = self.read_byte(addr);
                self.adc(v)
            }

            // ADC (zp),Y
            0x71 => {
                let addr = self.indirect_indexed();
                let v = self.read_byte(addr);
                self.adc(v)
            }

            // SBC #imm
            0xE9 => {
                let addr = self.immediate();
                let v = self.read_byte(addr);
                self.sbc(v)
            }

            // SBC zp
            0xE5 => {
                let addr = self.zeropage();
                let v = self.read_byte(addr);
                self.sbc(v)
            }

            // SBC zp,X
            0xF5 => {
                let addr = self.zeropage_x();
                let v = self.read_byte(addr);
                self.sbc(v)
            }

            // SBC abs
            0xED => {
                let addr = self.absolute();
                let v = self.read_byte(addr);
                self.sbc(v)
            }

            // SBC abs,X
            0xFD => {
                let addr = self.absolute_x();
                let v = self.read_byte(addr);
                self.sbc(v)
            }

            // SBC abs,Y
            0xF9 => {
                let addr = self.absolute_y();
                let v = self.read_byte(addr);
                self.sbc(v)
            }

            // SBC (zp,X)
            0xE1 => {
                let addr = self.indexed_indirect();
                let v = self.read_byte(addr);
                self.sbc(v)
            }

            // SBC (zp),Y
            0xF1 => {
                let addr = self.indirect_indexed();
                let v = self.read_byte(addr);
                self.sbc(v)
            }

            // INC zp
            0xE6 => {
                let addr = self.zeropage();
                self.inc(addr)
            }

            // INC zp,X
            0xF6 => {
                let addr = self.zeropage_x();
                self.inc(addr)
            }

            // INC abs
            0xEE => {
                let addr = self.absolute();
                self.inc(addr)
            }

            // INC abs,X
            0xFE => {
                let addr = self.absolute_x();
                self.inc(addr)
            }

            // DEC zp
            0xC6 => {
                let addr = self.zeropage();
                self.dec(addr)
            }

            // DEC zp,X
            0xD6 => {
                let addr = self.zeropage_x();
                self.dec(addr)
            }

            // DEC abs
            0xCE => {
                let addr = self.absolute();
                self.dec(addr)
            }

            // DEC abs,X
            0xDE => {
                let addr = self.absolute_x();
                self.dec(addr)
            }

            // CMP #imm
            0xC9 => {
                let addr = self.immediate();
                self.cmp(self.a, self.read_byte(addr))
            }

            // CMP zp
            0xC5 => {
                let addr = self.zeropage();
                self.cmp(self.a, self.read_byte(addr))
            }

            // CMP zp,X
            0xD5 => {
                let addr = self.zeropage_x();
                self.cmp(self.a, self.read_byte(addr))
            }

            // CMP abs
            0xCD => {
                let addr = self.absolute();
                self.cmp(self.a, self.read_byte(addr))
            }

            // CMP abs,X
            0xDD => {
                let addr = self.absolute_x();
                self.cmp(self.a, self.read_byte(addr))
            }

            // CMP abs,Y
            0xD9 => {
                let addr = self.absolute_y();
                self.cmp(self.a, self.read_byte(addr))
            }

            // CMP (zp,X)
            0xC1 => {
                let addr = self.indexed_indirect();
                self.cmp(self.a, self.read_byte(addr))
            }

            // CMP (zp),Y
            0xD1 => {
                let addr = self.indirect_indexed();
                self.cmp(self.a, self.read_byte(addr))
            }

            // CPX #imm
            0xE0 => {
                let addr = self.immediate();
                self.cmp(self.x, self.read_byte(addr))
            }

            // CPX zp
            0xE4 => {
                let addr = self.zeropage();
                self.cmp(self.x, self.read_byte(addr))
            }

            // CPX abs
            0xEC => {
                let addr = self.absolute();
                self.cmp(self.x, self.read_byte(addr))
            }

            // CPY #imm
            0xC0 => {
                let addr = self.immediate();
                self.cmp(self.y, self.read_byte(addr))
            }

            // CPY zp
            0xC4 => {
                let addr = self.zeropage();
                self.cmp(self.y, self.read_byte(addr))
            }

            // CPY abs
            0xCC => {
                let addr = self.absolute();
                self.cmp(self.y, self.read_byte(addr))
            }

            // LDA #imm
            0xA9 => {
                let addr = self.immediate();
                let v = self.read_byte(addr);
                self.a = v;
                self.update_zn(v)
            }

            // LDA zp
            0xA5 => {
                let addr = self.zeropage();
                let v = self.read_byte(addr);
                self.a = v;
                self.update_zn(v)
            }

            // LDA zp,X
            0xB5 => {
                let addr = self.zeropage_x();
                let v = self.read_byte(addr);
                self.a = v;
                self.update_zn(v)
            }

            // LDA abs
            0xAD => {
                let addr = self.absolute();
                let v = self.read_byte(addr);
                self.a = v;
                self.update_zn(v)
            }

            // LDA abs,X
            0xBD => {
                let addr = self.absolute_x();
                let v = self.read_byte(addr);
                self.a = v;
                self.update_zn(v)
            }

            // LDA abs,Y
            0xB9 => {
                let addr = self.absolute_y();
                let v = self.read_byte(addr);
                self.a = v;
                self.update_zn(v)
            }

            // LDA (zp,X)
            0xA1 => {
                let addr = self.indexed_indirect();
                let v = self.read_byte(addr);
                self.a = v;
                self.update_zn(v)
            }

            // LDA (zp),Y
            0xB1 => {
                let addr = self.indirect_indexed();
                let v = self.read_byte(addr);
                self.a = v;
                self.update_zn(v)
            }

            // LDX #imm
            0xA2 => {
                let addr = self.immediate();
                let v = self.read_byte(addr);
                self.x = v;
                self.update_zn(v)
            }

            // LDX zp
            0xA6 => {
                let addr = self.zeropage();
                let v = self.read_byte(addr);
                self.x = v;
                self.update_zn(v)
            }

            // LDX zp,Y
            0xB6 => {
                let addr = self.zeropage_y();
                let v = self.read_byte(addr);
                self.x = v;
                self.update_zn(v)
            }

            // LDX abs
            0xAE => {
                let addr = self.absolute();
                let v = self.read_byte(addr);
                self.x = v;
                self.update_zn(v)
            }

            // LDX abs,Y
            0xBE => {
                let addr = self.absolute_y();
                let v = self.read_byte(addr);
                self.x = v;
                self.update_zn(v)
            }

            // LDY #imm
            0xA0 => {
                let addr = self.immediate();
                let v = self.read_byte(addr);
                self.y = v;
                self.update_zn(v)
            }

            // LDY zp
            0xA4 => {
                let addr = self.zeropage();
                let v = self.read_byte(addr);
                self.y = v;
                self.update_zn(v)
            }

            // LDY zp,X
            0xB4 => {
                let addr = self.zeropage_x();
                let v = self.read_byte(addr);
                self.y = v;
                self.update_zn(v)
            }

            // LDY abs
            0xAC => {
                let addr = self.absolute();
                let v = self.read_byte(addr);
                self.y = v;
                self.update_zn(v)
            }

            // LDY abs,X
            0xBC => {
                let addr = self.absolute_x();
                let v = self.read_byte(addr);
                self.y = v;
                self.update_zn(v)
            }

            // STA zp
            0x85 => {
                let addr = self.zeropage();
                self.write_byte(addr, self.a)
            }

            // STA zp,X
            0x95 => {
                let addr = self.zeropage_x();
                self.write_byte(addr, self.a)
            }

            // STA abs
            0x8D => {
                let addr = self.absolute();
                self.write_byte(addr, self.a)
            }

            // STA abs,X
            0x9D => {
                let addr = self.absolute_x();
                self.write_byte(addr, self.a)
            }

            // STA abs,Y
            0x99 => {
                let addr = self.absolute_y();
                self.write_byte(addr, self.a)
            }

            // STA (zp,X)
            0x81 => {
                let addr = self.indexed_indirect();
                self.write_byte(addr, self.a)
            }

            // STA (zp),Y
            0x91 => {
                let addr = self.indirect_indexed();
                self.write_byte(addr, self.a)
            }

            // STX zp
            0x86 => {
                let addr = self.zeropage();
                self.write_byte(addr, self.x)
            }

            // STX zp,Y
            0x96 => {
                let addr = self.zeropage_y();
                self.write_byte(addr, self.x)
            }

            // STX abs
            0x8E => {
                let addr = self.absolute();
                self.write_byte(addr, self.x)
            }

            // STY zp
            0x84 => {
                let addr = self.zeropage();
                self.write_byte(addr, self.y)
            }

            // STY zp,X
            0x94 => {
                let addr = self.zeropage_x();
                self.write_byte(addr, self.y)
            }

            // STY abs
            0x8C => {
                let addr = self.absolute();
                self.write_byte(addr, self.y)
            }

            // TAX
            0xAA => {
                self.x = self.a;
                self.update_zn(self.x)
            }

            // TAY
            0xA8 => {
                self.y = self.a;
                self.update_zn(self.y)
            }

            // TXA
            0x8A => {
                self.a = self.x;
                self.update_zn(self.a)
            }

            // TYA
            0x98 => {
                self.a = self.y;
                self.update_zn(self.a)
            }

            // TSX
            0xBA => {
                self.x = self.sp;
                self.update_zn(self.x)
            }

            // TXS
            0x9A => self.sp = self.x,

            // PHA
            0x48 => self.push(self.a),

            // PLA
            0x68 => {
                let v = self.pop();
                self.a = v;
                self.update_zn(v)
            }

            // PHP
            0x08 => self.push(self.status | Self::STATUS_FLAG_B | Self::STATUS_FLAG_U),

            // PLP
            0x28 => {
                self.status = self.pop();
                self.status |= Self::STATUS_FLAG_U
            }

            // ASL A
            0x0A => self.asl_acc(),

            // ASL zp
            0x06 => {
                let addr = self.zeropage();
                self.asl_mem(addr)
            }

            // ASL zp,X
            0x16 => {
                let addr = self.zeropage_x();
                self.asl_mem(addr)
            }

            // ASL abs
            0x0E => {
                let addr = self.absolute();
                self.asl_mem(addr)
            }

            // ASL abs,X
            0x1E => {
                let addr = self.absolute_x();
                self.asl_mem(addr)
            }

            // LSR A
            0x4A => self.lsr_acc(),

            // LSR zp
            0x46 => {
                let addr = self.zeropage();
                self.lsr_mem(addr)
            }

            // LSR zp,X
            0x56 => {
                let addr = self.zeropage_x();
                self.lsr_mem(addr)
            }

            // LSR abs
            0x4E => {
                let addr = self.absolute();
                self.lsr_mem(addr)
            }

            // LSR abs,X
            0x5E => {
                let addr = self.absolute_x();
                self.lsr_mem(addr)
            }

            // ROL A
            0x2A => self.rol_acc(),

            // ROL zp
            0x26 => {
                let addr = self.zeropage();
                self.rol_mem(addr)
            }

            // ROL zp,X
            0x36 => {
                let addr = self.zeropage_x();
                self.rol_mem(addr)
            }

            // ROL abs
            0x2E => {
                let addr = self.absolute();
                self.rol_mem(addr)
            }

            // ROL abs,X
            0x3E => {
                let addr = self.absolute_x();
                self.rol_mem(addr)
            }

            // ROR A
            0x6A => self.ror_acc(),

            // ROR zp
            0x66 => {
                let addr = self.zeropage();
                self.ror_mem(addr)
            }

            // ROR zp,X
            0x76 => {
                let addr = self.zeropage_x();
                self.ror_mem(addr)
            }

            // ROR abs
            0x6E => {
                let addr = self.absolute();
                self.ror_mem(addr)
            }

            // ROR abs,X
            0x7E => {
                let addr = self.absolute_x();
                self.ror_mem(addr)
            }

            // NOP
            0xEA => {}

            // Temp behavior for unimplemented opcodes
            _op => {}
        }
    }
}

impl core::fmt::Debug for CentralProcessingUnit {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        writeln!(f, "6502 CPU State:")?;
        writeln!(f, "  PC     : {:#06X}", self.pc)?;
        writeln!(f, "  A      : {:#04X}", self.a)?;
        writeln!(f, "  X      : {:#04X}", self.x)?;
        writeln!(f, "  Y      : {:#04X}", self.y)?;
        writeln!(f, "  SP     : {:#04X}", self.sp)?;
        writeln!(f, "  STATUS : {:#04X}", self.status)?;

        Ok(())
    }
}

impl core::fmt::Display for CentralProcessingUnit {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        writeln!(f, "6502 CPU State:")?;
        writeln!(f, "  PC     : {:#06X}", self.pc)?;
        writeln!(f, "  A      : {:#04X}", self.a)?;
        writeln!(f, "  X      : {:#04X}", self.x)?;
        writeln!(f, "  Y      : {:#04X}", self.y)?;
        writeln!(f, "  SP     : {:#04X}", self.sp)?;
        writeln!(f, "  STATUS : {:#04X}", self.status)?;

        Ok(())
    }
}
