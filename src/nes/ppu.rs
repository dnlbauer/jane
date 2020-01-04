use crate::nes::bus::Memory;
use crate::nes::bus::Clockable;
use image::{ImageBuffer, Rgba};
use rand::Rng;

pub struct PPU {
    pub cycle: i16, 
    pub scanline: i16,
    pub canvas_main: ImageBuffer<Rgba<u8>, Vec<u8>>

}

impl PPU {
    pub fn new() -> Self {
        PPU {
            cycle: 0,
            scanline: 0,
            canvas_main: ImageBuffer::new(256, 240),
        }
    }
}

impl Clockable for PPU {
    fn clock<T: Memory>(&mut self, _bus: &mut T) {
        // random noise
        let mut rng = rand::thread_rng();
        let x = rng.gen_range(0, 256);
        let y = rng.gen_range(0, 240);
        
        let color = rng.gen::<bool>();
        let px = if color {
            Rgba([255, 255, 255, 255])
        } else {
            Rgba([0, 0, 0, 255])
        };

        self.canvas_main.put_pixel(x as u32, y as u32, px);
    } 
}



