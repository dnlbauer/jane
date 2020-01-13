use crate::nes::memory::{Memory,PPUMemoryReader,PPUMemory};
use crate::nes::types::*;
use image::{ImageBuffer, Rgba};
use rand::Rng;
use palette::PALETTE;

pub mod palette;

pub type Pixel = Rgba<u8>;
pub type Sprite = ImageBuffer<Pixel, Vec<u8>>;


pub const CTRL_NAMETBL_X: Byte           = 1 << 0;
pub const CTRL_NAMETBL_Y: Byte           = 1 << 1;
pub const CTRL_INCR_MODE: Byte           = 1 << 2;
pub const CTRL_PATTERN_SPRITE: Byte      = 1 << 3;
pub const CTRL_PATTERN_BG: Byte          = 1 << 4;
pub const CTRL_SPRITE_SIZE: Byte         = 1 << 5;
pub const CTRL_SLAVE_MODE: Byte          = 1 << 6;
pub const CTRL_ENABLE_NMI: Byte          = 1 << 7;

pub const STATUS_SPRITE_OVERFOL: Byte    = 1 << 5;
pub const STATUS_SPRITE_ZERO_HIT: Byte    = 1 << 6;
pub const STATUS_VERTICAL_BLANK: Byte     = 1 << 7;

pub const MASK_GRAYSCALE: Byte           = 1 << 0;
pub const MASK_RENDER_BG_LEFT: Byte      = 1 << 1;
pub const MASK_RENDER_SPRITES_LEFT: Byte = 1 << 2;
pub const MASK_RENDER_BG: Byte           = 1 << 3;
pub const MASK_RENDER_SPRITES: Byte      = 1 << 4;
pub const MASK_ENHANCE_RED: Byte         = 1 << 5;
pub const MASK_ENHANCE_GREEN: Byte       = 1 << 6;
pub const MASK_ENHANCE_BLUE: Byte        = 1 << 7;

enum PPURegister {
    Control,
    Mask,
    Status,
    OAMAddr,
    OAMData,
    Scroll,
    Addr,
    Data,
    DMA 
}

pub struct Registers {
    // 0x2000
    pub ctrl: Byte,
    // 0x2001
    pub mask: Byte,
    // 0x2002
    pub status: Byte,
    // 0x2003
    pub oam_addr: Byte,
    // 0x2004
    pub oam_data: Byte,
    // 0x2005
    pub scroll: Byte,
    // 0x2006
    pub addr: Byte,
    // 0x2007
    pub data: Byte, 
    // 0x2008
    pub dma: Byte,  // 0x4014 
}

impl Registers {
    fn new() -> Registers {
        Registers {
           ctrl: 0x00,
           mask: 0x00,
           status: 0x00,
           oam_addr: 0x00,
           oam_data: 0x00,
           scroll: 0x00,
           addr: 0x00,
           data: 0x00,
           dma: 0x00, 
        }
    }
}

pub struct PPU {
    pub regs: Registers,
    pub cycle: u16, 
    pub scanline: u16,
    pub canvas_main: Sprite,
    pattern_table: [Sprite; 2],
    pub frame_ready: bool
}

impl PPUMemoryReader for PPU {}

impl PPU {
    pub fn new() -> Self {
        PPU {
            regs: Registers::new(),
            cycle: 0,
            scanline: 0,
            canvas_main: ImageBuffer::new(256, 240),
            pattern_table: [ImageBuffer::new(128, 128), ImageBuffer::new(128, 128)],
            frame_ready: false,
        }
    }

    pub fn reset(&mut self) {
        self.regs = Registers::new();
    }

    // Set a flag with the corresponding mask
    fn set_flag(&mut self, register: PPURegister, flag: Byte, val: bool) {
        // TODO Likely not all of them should be rw and treated as flags 
        let reg = match register {
            PPURegister::Control => &mut self.regs.ctrl,
            PPURegister::Mask => &mut self.regs.mask,
            PPURegister::Status => &mut self.regs.status,
            PPURegister::OAMAddr => &mut self.regs.oam_addr,
            PPURegister::OAMData => &mut self.regs.oam_data,
            PPURegister::Scroll => &mut self.regs.scroll,
            PPURegister::Addr => &mut self.regs.addr,
            PPURegister::Data => &mut self.regs.data,
            PPURegister::DMA => &mut self.regs.dma,
        };
            
        if val {
            *reg |= flag;
        } else {
            *reg &= !flag;
        }
    }


    // PPU renders 262 scanlines with 341 clocks per line. One px per clock
    // Scanline -1,261: Dummy scanline
    // Scanline 0-239: Visible scanlines:
    //      Cycle 0: idle.
    //      Cycle 1-256: Fetch tile data
    //      Cycle 257-320: Fetch tile data of sprites for next scanline 
    //      Cycle 321-336: Fetch first two tiles of next scanline
    //      Cycle 337-340: "Unknown" data fetch  
    // Scanline 240: PPU idle
    // Scanline 241-260: Vblack. Flag is set during second clock of 241 together
    // with NMI 
    pub fn clock<T: Memory>(&mut self, _mem: &mut T) {
        if self.cycle == 340 {
            self.cycle = 0;
            if self.scanline == 261 {
                self.scanline = 0;
                self.frame_ready = true;
            } else {
                self.scanline += 1;
            }
        } else {
            self.cycle += 1;
        };

        // set/clear vblank flag
        if self.scanline == 241 && self.cycle == 1 {
            self.set_flag(PPURegister::Status, STATUS_VERTICAL_BLANK, true);

        } else if self.scanline == 261 && self.cycle == 1 {
            self.set_flag(PPURegister::Status, STATUS_VERTICAL_BLANK, false);
        }
        
        // if (self.cycle < self.canvas_main.width() as u16) && 
        //     (self.scanline < self.canvas_main.height() as u16) {
        //     // random noise
        //     let mut rng = rand::thread_rng();
        //     let x = self.cycle;
        //     let y = self.scanline;
        
        //     let color = rng.gen::<bool>();
        //     let px = if color {
        //         PALETTE[&0x0f]
        //     } else {
        //         PALETTE[&0x20]
        //     };

        //     self.canvas_main.put_pixel(x as u32, y as u32, px);
        // }

    }

    // Get the correct color for the pixel from the given palette
    fn get_color(&self, pixel: Byte, _palette: Byte) -> Pixel {
        // TODO: not implemented yet. returns some black and white color
        match pixel {
            0 => PALETTE[&0x30],
            1 => PALETTE[&0x3d],
            2 => PALETTE[&0x2d],
            _ => PALETTE[&0x3f]
        }
    }

    // Get one of the two pattern tables of the PPU
    // This also initializes/updates the pattern table
    fn get_pattern_table<T: PPUMemory>(&mut self, index: usize, _palette: Byte, mem: &T) -> &Sprite {
        // 16 x 16 tiles of 8x8px sprites per pattern table => 128x128px
        for x in 0..16 {  // tile row
            for y in 0..16 {  // tile column
                // byte offset in pattern mem. Each row is 256 bytes
                // and each pixel is 16 bytes
                let tile_offset_b = y*256 + x*16;  

                // iterate over individual pixels of one tile
                for row in 0..8 { 
                    let tile_addr = index as Addr * 0x1000 + tile_offset_b + row;

                    // NES memory organization: The pattern table defines the
                    // two least significant bits of the color (value between
                    // 0-3). Two bitplanes in memory each having one byte per
                    // row 
                    let mut tile_lsb = mem.readb_ppu(tile_addr);
                    let mut tile_msb = mem.readb_ppu(tile_addr + 8);
                    for col in 0..8 {
                        let pixel = (tile_lsb & 0x01) + (tile_msb & 0x01);
                        self.pattern_table[index].put_pixel(
                            (x * 8 + (7-col)) as u32, // x starts on the right, sprit is from left
                            (y * 8 + row) as u32,
                            self.get_color(123, pixel));
                        tile_lsb >>= 1;
                        tile_msb >>= 1;
                    } 
                } 
            }
        }

        &self.pattern_table[index]
    }
}

impl Memory for PPU {
    fn readb(&self, addr: Addr) -> Byte {
       // Only some of the PPU regs can actually by read
       match addr {
           // PPU status 
           0x2002 => { return self.regs.status },
           // OAM data (object attribute memory = list of 64 sprites) 
           0x2004 => { unimplemented!() },
           // ppu data 
           0x2007 => { unimplemented!() },
           _ => 0x00  
       } 
        
    }
    fn writeb(&mut self, addr: Addr, data: Byte) {
        // Only some of the PPU regs can be written to
        match addr {
            // Control 
            0x2000 => { self.regs.ctrl = data },
            // Mask 
            0x2001 => { self.regs.mask = data },
            // OAM address
            0x2003 => { unimplemented!() },
            // OAM data
            0x2004 => { unimplemented!() },
            // Scroll
            0x2005 => { self.regs.scroll = data },
            // Address
            0x2006 => { self.regs.addr = data }, 
            // ppu data
            0x2007 => { self.regs.data = data },
            _ => { unreachable!() } 
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_set_flags() {
        let mut ppu = PPU::new();

        // set unset flag
        assert_eq!(ppu.regs.status, 0b00000000);
        ppu.set_flag(PPURegister::Status, STATUS_VERTICAL_BLANK, true);
        assert_eq!(ppu.regs.status, 0b10000000,
            "register={:#010b}; should be 0b10000000", ppu.regs.status);
        ppu.set_flag(PPURegister::Status, STATUS_VERTICAL_BLANK, false);
        assert_eq!(ppu.regs.status, 0b00000000,
            "register={:#010b}; should be 0b00000000", ppu.regs.status);


        // set same flag twice
        ppu.set_flag(PPURegister::Status, STATUS_VERTICAL_BLANK, true);
        assert_eq!(ppu.regs.status, 0b10000000,
            "register={:#010b}; should be 0b10000000", ppu.regs.status);
        ppu.set_flag(PPURegister::Status, STATUS_VERTICAL_BLANK, true);
        assert_eq!(ppu.regs.status, 0b10000000,
            "register={:#010b}; should be 0b10000000", ppu.regs.status);


        // set other flag
        ppu.set_flag(PPURegister::Status, STATUS_SPRITE_ZERO_HIT, true);
        assert_eq!(ppu.regs.status, 0b11000000,
            "register={:#010b}; should be 0b11000000", ppu.regs.status);

        ppu.set_flag(PPURegister::Status, STATUS_SPRITE_ZERO_HIT, false);
        assert_eq!(ppu.regs.status, 0b10000000,
            "register={:#010b}; should be 0b10000000", ppu.regs.status);


        // set other register
        ppu.set_flag(PPURegister::Mask, MASK_GRAYSCALE, true);
        assert_eq!(ppu.regs.status, 0b10000000,
            "register={:#010b}; should be 0b10000000", ppu.regs.status);
        assert_eq!(ppu.regs.mask, 0b0000001,
            "register={:#010b}; should be 0b0000001", ppu.regs.status);
    }
}
