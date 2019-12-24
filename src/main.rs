#[macro_use] extern crate log;
#[macro_use] extern crate failure;
extern crate simple_logger;
extern crate phf;
extern crate piston_window;

mod nes; 

use std::path::Path;
use piston_window::*;
use nes::types::*;
use nes::cpu::*;
use nes::bus::*;
use nes::disasm::*;
use opengl_graphics::OpenGL;
use log::Level;
use piston_window::Text;

fn main() {
    simple_logger::init_with_level(Level::Debug).unwrap();
    let mut window: PistonWindow = WindowSettings::new("NESemu", [256*6, 240*4])
        .exit_on_esc(true).graphics_api(OpenGL::V3_2).build().unwrap();

    let mut bus = MemoryBus::new();

    // Load little test program into ram
    let test_prog: [Byte; 28] = [0xA2, 0x0A, 0x8E, 0x00, 0x00, 0xA2, 0x03, 0x8E, 0x01, 0x00, 0xAC, 0x00, 0x00, 0xA9,
    0x00, 0x18, 0x6D, 0x01, 0x00, 0x88, 0xD0, 0xFA, 0x8D, 0x02, 0x00, 0xEA, 0xEA, 0xEA];
    let offset: Addr = 0x8000;
    for i in 0..test_prog.len() {
        let addr = (i as Addr) + offset;
        bus.writeb(addr, test_prog[i])
    }

    // hint program location to processor
    bus.writew(0xfffc, offset);

    // disassemble instructions
    let disasm = Disasm::disassemble(&test_prog, offset).unwrap();

    // Create and reset CPU 
    let mut cpu: CPU = CPU::new();
    cpu.reset(&bus);

    // Load font and start main loop
    let font_path = Path::new("resources/fonts/PressStart2P.ttf");
    let mut glyphs = window.load_font(font_path).unwrap();
    while let Some(event) = window.next() {
        if let Some(Button::Keyboard(key)) = event.press_args() {
            if key == Key::C {  // advance one clock
                cpu.clock(&mut bus);
            } else if key == Key::Space {  // advance to next instruction
                cpu.clock(&mut bus);
                while cpu.is_ahead() {
                    cpu.clock(&mut bus);
                }
            }
        } 
        render(&mut window, &event, &mut glyphs, &cpu, &bus, &disasm);
    }
}

fn render(window: &mut PistonWindow, event: &Event, glyphs: &mut Glyphs, cpu: &CPU, bus: &dyn Bus, disasm: &Disasm) {
    window.draw_2d(event, |c, g, d| {
        clear([0.0, 0.0, 1.0, 1.0], g);
        render_cpu(&c, g, glyphs, cpu, disasm);
        render_pc(&c, g, glyphs, cpu, bus);
        glyphs.factory.encoder.flush(d); 
    });
}

fn render_cpu(c: &Context, g: &mut G2d, glyphs: &mut Glyphs, cpu: &CPU, disasm: &Disasm) {
        let font_size_pt = 36;
        let color_white = [1.0; 4];
        let color_red = [1.0, 0.0, 0.0, 1.0];
        let color_green = [0.0, 1.0, 0.0, 1.0];
        
        let font_scale = 0.25;
        let font_size_px = 12;

        let line_distance = font_size_px as f64 * 0.5;
        let x_dist = font_size_px as f64;

        let flags_y = line_distance + font_size_px as f64;
        let mut flags_x_offset = x_dist;
        let mut transform = c.transform.trans(x_dist, flags_y).scale(font_scale, font_scale);
        Text::new_color(color_white, font_size_pt).draw(
                &"Flags:",
                glyphs,
                &c.draw_state,
                transform,
                g
            );
        transform = transform.trans(300.0, 0.0);
        Text::new_color(if cpu.get_flag(NEGATIVE) == 1 { color_green} else { color_red } , font_size_pt).draw(
                &"N",
                glyphs,
                &c.draw_state,
                transform,
                g
            );
        transform = transform.trans(72.0, 0.0);
        Text::new_color(if cpu.get_flag(OVERFLOW) == 1 { color_green} else { color_red } , font_size_pt).draw(
                &"V",
                glyphs,
                &c.draw_state,
                transform,
                g
            );
        transform = transform.trans(72.0, 0.0);
        Text::new_color(color_white, font_size_pt).draw(
                &"-",
                glyphs,
                &c.draw_state,
                transform,
                g
            );
        transform = transform.trans(72.0, 0.0);
        Text::new_color(if cpu.get_flag(BREAK) == 1 { color_green} else { color_red } , font_size_pt).draw(
                &"B",
                glyphs,
                &c.draw_state,
                transform,
                g
            );
        transform = transform.trans(72.0, 0.0);
        Text::new_color(if cpu.get_flag(DECIMAL) == 1 { color_green} else { color_red } , font_size_pt).draw(
                &"D",
                glyphs,
                &c.draw_state,
                transform,
                g
            );
        transform = transform.trans(72.0, 0.0);
        Text::new_color(if cpu.get_flag(IRQ) == 1 { color_green} else { color_red } , font_size_pt).draw(
                &"I",
                glyphs,
                &c.draw_state,
                transform,
                g
            );
        transform = transform.trans(72.0, 0.0);
        Text::new_color(if cpu.get_flag(ZERO) == 1 { color_green} else { color_red } , font_size_pt).draw(
                &"Z",
                glyphs,
                &c.draw_state,
                transform,
                g
            );
        transform = transform.trans(72.0, 0.0);
        Text::new_color(if cpu.get_flag(CARRY) == 1 { color_green} else { color_red } , font_size_pt).draw(
                &"C",
                glyphs,
                &c.draw_state,
                transform,
                g
            );

        let cpu_register_texts = [
            &format!("A: {:#x} ({})", cpu.regs.a, cpu.regs.a),
            &format!("X: {:#x} ({})", cpu.regs.x, cpu.regs.y),
            &format!("Y: {:#x} ({})", cpu.regs.y, cpu.regs.x),
            &format!("Stack P: {:#x} ({})", cpu.regs.sp, cpu.regs.sp),
            &format!("Program P: {:#x}", cpu.regs.pc),
        ]; 

        for (i, &text) in cpu_register_texts.iter().enumerate() {
            let y_dist: f64 = (i+2) as f64*(line_distance + font_size_px as f64);
            let transform = c.transform.trans(x_dist, y_dist).scale(font_scale, font_scale);
            Text::new_color(color_white, font_size_pt).draw(
                &text,
                glyphs,
                &c.draw_state,
                transform,
                g
            );
        }

        for (i, text) in disasm.instructions.iter().enumerate() {
            let y_dist: f64 = (i+8) as f64*(line_distance + font_size_px as f64);
            let transform = c.transform.trans(x_dist, y_dist).scale(font_scale, font_scale);
            Text::new_color(color_white, font_size_pt).draw(
                &text,
                glyphs,
                &c.draw_state,
                transform,
                g
            );
        }


}

fn render_pc(c: &Context, g: &mut G2d, glyphs: &mut Glyphs, cpu: &CPU, bus: &dyn Bus) {

}