use crate::nes::memory::PPUMemory;
use phf::{Map,phf_map};
use crate::nes::types::*;
use crate::nes::memory::Memory;
use image::{ImageBuffer, Rgba};
use rand::Rng;

type Pixel = Rgba<u8>;
type Sprite = ImageBuffer<Pixel, Vec<u8>>;

static PALETTE: Map<u8, Pixel> = phf_map! {
    // 0x00
    0x00u8 => Rgba([84, 84, 84, 255]),
    0x01u8 => Rgba([0, 30, 116, 255]),
    0x02u8 => Rgba([8, 16, 144, 255]),
    0x03u8 => Rgba([48, 0, 136, 255]),
    0x04u8 => Rgba([68, 0, 100, 255]),
    0x05u8 => Rgba([92, 0, 48, 255]),
    0x06u8 => Rgba([84, 4, 0, 255]),
    0x07u8 => Rgba([60, 24, 0, 255]),
    0x08u8 => Rgba([32, 42, 0, 255]),
    0x09u8 => Rgba([8, 58, 0, 255]),
    0x0au8 => Rgba([0, 64, 0, 255]),
    0x0bu8 => Rgba([0, 60, 0, 255]),
    0x0cu8 => Rgba([0, 50, 60, 255]),
    0x0du8 => Rgba([0, 0, 0, 255]),
    0x0eu8 => Rgba([0, 0, 0, 255]),
    0x0fu8 => Rgba([0, 0, 0, 255]),
    // 0x10
    0x10u8 => Rgba([152, 150, 152, 255]),
    0x11u8 => Rgba([8, 76, 196, 255]),
    0x12u8 => Rgba([48, 50, 236, 255]),
    0x13u8 => Rgba([92, 30, 228, 255]),
    0x14u8 => Rgba([136, 20, 176, 255]),
    0x15u8 => Rgba([160, 20, 100, 255]),
    0x16u8 => Rgba([152, 34, 32, 255]),
    0x17u8 => Rgba([120, 60, 0, 255]),
    0x18u8 => Rgba([84, 90, 0, 255]),
    0x19u8 => Rgba([40, 114, 0, 255]),
    0x1au8 => Rgba([8, 124, 0, 255]),
    0x1bu8 => Rgba([0, 118, 40, 255]),
    0x1cu8 => Rgba([0, 102, 120, 255]),
    0x1du8 => Rgba([0, 0, 0, 255]),
    0x1eu8 => Rgba([0, 0, 0, 255]),
    0x1fu8 => Rgba([0, 0, 0, 255]),
    // 0x20
    0x20u8 => Rgba([236, 238, 236, 255]),
    0x21u8 => Rgba([76, 154, 236, 255]),
    0x22u8 => Rgba([120, 124, 236, 255]),
    0x23u8 => Rgba([176, 98, 236, 255]),
    0x24u8 => Rgba([228, 84, 236, 255]),
    0x25u8 => Rgba([236, 88, 180, 255]),
    0x26u8 => Rgba([236, 106, 100, 255]),
    0x27u8 => Rgba([212, 136, 32, 255]),
    0x28u8 => Rgba([160, 170, 0, 255]),
    0x29u8 => Rgba([116, 196, 0, 255]),
    0x2au8 => Rgba([76, 208, 32, 255]),
    0x2bu8 => Rgba([56, 204, 108, 255]),
    0x2cu8 => Rgba([56, 180, 204, 255]),
    0x2du8 => Rgba([60, 60, 60, 255]),
    0x2eu8 => Rgba([0, 0, 0, 255]),
    0x2fu8 => Rgba([0, 0, 0, 255]),
    // 0x30
    0x30u8 => Rgba([236, 238, 236, 255]),
    0x31u8 => Rgba([168, 204, 236, 255]),
    0x32u8 => Rgba([188, 188, 236, 255]),
    0x33u8 => Rgba([212, 178, 236, 255]),
    0x34u8 => Rgba([236, 174, 236, 255]),
    0x35u8 => Rgba([236, 174, 212, 255]),
    0x36u8 => Rgba([236, 180, 176, 255]),
    0x37u8 => Rgba([228, 196, 144, 255]),
    0x38u8 => Rgba([204, 210, 120, 255]),
    0x39u8 => Rgba([180, 222, 120, 255]),
    0x3au8 => Rgba([168, 226, 144, 255]),
    0x3bu8 => Rgba([152, 226, 180, 255]),
    0x3cu8 => Rgba([160, 214, 228, 255]),
    0x3du8 => Rgba([160, 162, 160, 255]),
    0xe3u8 => Rgba([0, 0, 0, 255]),
    0x3fu8 => Rgba([0, 0, 0, 255]),
};

struct Registers {
    // 0x2000
    ctrl: Word,
    // 0x2001
    mask: Word,
    // 0x2002
    status: Word,
    // 0x2003
    oam_addr: Word,
    // 0x2004
    oam_data: Word,
    // 0x2005
    scroll: Word,
    // 0x2006
    addr: Word,
    // 0x2007
    data: Word, 
    // 0x2008
    dma: Word,  // 0x4014 
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
    registers: Registers,
    pub cycle: u16, 
    pub scanline: u16,
    pub canvas_main: Sprite,
    pattern_table: [Sprite; 2],
    pub frame_ready: bool

}

impl PPU {
    pub fn new() -> Self {
        PPU {
            registers: Registers::new(),
            cycle: 0,
            scanline: 0,
            canvas_main: ImageBuffer::new(256, 240),
            pattern_table: [ImageBuffer::new(128, 128), ImageBuffer::new(128, 128)],
            frame_ready: false,
        }
    }

    pub fn reset(&mut self) {
        self.registers = Registers::new();
    }

    pub fn clock<T: Memory>(&mut self, _mem: &mut T) {
        if (self.cycle < self.canvas_main.width() as u16) && 
            (self.scanline < self.canvas_main.height() as u16) {
            // random noise
            let mut rng = rand::thread_rng();
            let x = self.cycle;
            let y = self.scanline;
        
            let color = rng.gen::<bool>();
            let px = if color {
                PALETTE[&0x0f]
            } else {
                PALETTE[&0x20]
            };

            self.canvas_main.put_pixel(x as u32, y as u32, px);
        }

        // increment cycle/scanline
        self.cycle += 1;
        if self.cycle > 340 {
            self.cycle = 0;
            self.scanline += 1;
            if self.scanline >= 261 {
                self.scanline = 0;
                self.frame_ready = true;
            } 
        }
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
       // Only some of the PPU registers can actually by read
       match addr {
           // PPU status 
           0x2002 => { 0x00 },
           // OAM data (object attribute memory = list of 64 sprites) 
           0x2004 => { unimplemented!() },
           // ppu data 
           0x2007 => { unimplemented!() },
           _ => 0x00  
       } 
        
    }
    fn writeb(&mut self, addr: Addr, data: Byte) {
        // Only some of the PPU registers can be written to
        match addr {
            // Control 
            0x2000 => { unimplemented!() },
            // Mask 
            0x2001 => { unimplemented!() },
            // OAM address
            0x2003 => { unimplemented!() },
            // OAM data
            0x2004 => { unimplemented!() },
            // Scroll
            0x2005 => { unimplemented!() },
            // Address
            0x2006 => { unimplemented!() }, 
            // ppu data
            0x2007 => { unimplemented!() },
            _ => { } 
        }
    }
}
