use gfx_device_gl::Factory;
use gfx_device_gl::Resources;
use crate::nes::cpu::CPU;
use gfx_glyph::{Section,GlyphBrush};

pub trait Draw {
    fn draw(&mut self, glyphs: &mut GlyphBrush<Resources,Factory>);
}

pub struct CPURenderer<'a> {
    registers_text: String,
    registers_section: Section<'a>,
}

impl<'a> CPURenderer<'a> {
    pub fn new(cpu: &CPU) -> Self {
        let registers_text = String::from("A: 0x00 (000)");
        let registers_section = Section {
            text: "xxx",
            screen_position: (100.0, 100.0),
            color: [1.0; 4], 
            ..Section::default()
        };
        CPURenderer {
            registers_text: registers_text,
            registers_section: registers_section, 
        }
    }
}

impl<'a> CPURenderer<'a> {
    pub fn update(&mut self, cpu: &CPU) {
        self.registers_text.replace_range(3..7, "txtx");
        // self.registers_section.text = &self.registers_text;
    }
}

impl<'a> Draw for CPURenderer<'a> {
    fn draw(&mut self, glyphs: &mut GlyphBrush<Resources,Factory>) {
        glyphs.queue(self.registers_section);
    }
}