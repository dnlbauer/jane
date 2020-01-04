use crate::nes::bus::Memory;
use crate::nes::bus::Clockable;
use image::{ImageBuffer, Rgba};
use rand::Rng;

pub struct PPU {
    pub canvas_main: ImageBuffer<Rgba<u8>, Vec<u8>>
}

impl PPU {
    pub fn new() -> Self {
        PPU {
            canvas_main: ImageBuffer::new(256, 240),
        }
    }
}

impl Clockable for PPU {
    fn clock<T: Memory>(&mut self, bus: &mut T) {
        // update random pixel
        let mut rng = rand::thread_rng();
        let x = rng.gen_range(0, self.canvas_main.width());
        let y = rng.gen_range(0, self.canvas_main.height());
        let rgba = Rgba([rng.gen_range(0,255), rng.gen_range(0,255), rng.gen_range(0,255), 255]);
        self.canvas_main.put_pixel(x, y, rgba);
    } 
}



