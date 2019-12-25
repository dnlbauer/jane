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
use nes::cartridge::*;
use nes::disasm::*;
use opengl_graphics::OpenGL;
use log::Level;
use piston_window::Text;

// font options
const FT_SIZE_PT: u32 = 6;
const FT_SIZE_PX: f64 = 8.0;
const FT_COLOR_WHITE: [f32; 4] = [1.0; 4];
const FT_COLOR_RED: [f32; 4] = [1.0, 0.0, 0.0, 1.0];
const FT_COLOR_GREEN: [f32; 4] = [0.0, 1.0, 0.0, 1.0];
const FT_LINE_DISTANCE: f64 = FT_SIZE_PX * 0.5;

// font for text drawing
fn get_glyphs(window: &mut PistonWindow) -> Glyphs {
    let font_path = Path::new("resources/fonts/PressStart2P.ttf");
    window.load_font(font_path).unwrap()
}

fn main() {
    simple_logger::init_with_level(Level::Debug).unwrap();
   
    let cartridge = Path::new("test_roms/registers.nes");
    let cartridge = Cartridge::new(cartridge).unwrap();

    let mut bus = MemoryBus::new();
    bus.insert_cartrige(cartridge);

    // Load little test program into ram
    // let test_prog: [Byte; 28] = [0xA2, 0x0A, 0x8E, 0x00, 0x00, 0xA2, 0x03, 0x8E, 0x01, 0x00, 0xAC, 0x00, 0x00, 0xA9,
    // 0x00, 0x18, 0x6D, 0x01, 0x00, 0x88, 0xD0, 0xFA, 0x8D, 0x02, 0x00, 0xEA, 0xEA, 0xEA];
    // let offset: Addr = 0x8000;
    // for i in 0..test_prog.len() {
    //     let addr = (i as Addr) + offset;
    //     bus.writeb(addr, test_prog[i])
    // }
    // // hint program location to processor
    // bus.writew(0xfffc, offset);

    // disassemble instructions
    // let disasm = Disasm::disassemble(&test_prog, offset).unwrap();
    let disasm = Disasm::disassemble(&[], 0x000).unwrap();

    // Create and reset CPU 
    let mut cpu: CPU = CPU::new();
    cpu.reset(&bus);

    // Prepare window and drawing resources
    let mut window: PistonWindow = WindowSettings::new("NESemu", [256*3, 240*2])
        .exit_on_esc(true).graphics_api(OpenGL::V3_2).build().unwrap();
    let mut event_settings = EventSettings::new();
    event_settings.max_fps = 60;
    let mut events = Events::new(event_settings);
    let mut glyphs = get_glyphs(&mut window);

    // Main loop
    while let Some(event) = events.next(&mut window) {
        if let Some(_) = event.render_args() {
            render(&mut window, &event, &mut glyphs, &cpu, &bus, &disasm);
        }
        if let Some(Button::Keyboard(key)) = event.press_args() {
            match key {
                Key::C => cpu.clock(&mut bus),  // advance one clock
                Key::Space => {  // advance to next instruction
                    cpu.clock(&mut bus);
                    while cpu.is_ahead() {
                        cpu.clock(&mut bus);
                    }
                }
                _ => { }
            }
        }         
    }
}

fn render(window: &mut PistonWindow, event: &Event, glyphs: &mut Glyphs, cpu: &CPU, bus: &MemoryBus, disasm: &Disasm) {
    window.draw_2d(event, |c, g, d| {
        clear([0.0, 0.0, 1.0, 1.0], g);

        let debug_offset = [10.0, 10.0];
        render_cpu(&c, g, glyphs, cpu, debug_offset);
        render_disasm(&c, g, glyphs, disasm, cpu.regs.pc,
            [debug_offset[0], debug_offset[1] + (7.0 * (FT_LINE_DISTANCE+FT_SIZE_PX))]);
        render_memory(&c, g, glyphs, bus,
            [debug_offset[0] + 300.0, debug_offset[1]]);
        
        glyphs.factory.encoder.flush(d); 
    });
}

fn render_cpu(c: &Context, g: &mut G2d, glyphs: &mut Glyphs, cpu: &CPU, offset: [f64; 2]) {
        let mut transform = c.transform
            .trans(offset[0], offset[1] + FT_SIZE_PX);
        let mut color = FT_COLOR_WHITE;
        Text::new_color(color, FT_SIZE_PT).draw(
                &"Flags:",
                glyphs,
                &c.draw_state,
                transform,
                g
            ).unwrap();

        transform = transform.trans(70.0, 0.0);
        color = if cpu.get_flag(NEGATIVE) == 1 { FT_COLOR_GREEN} else { FT_COLOR_RED };
        Text::new_color(color , FT_SIZE_PT).draw(
                &"N",
                glyphs,
                &c.draw_state,
                transform,
                g
            ).unwrap();
        transform = transform.trans(16.0, 0.0);
        color = if cpu.get_flag(OVERFLOW) == 1 { FT_COLOR_GREEN} else { FT_COLOR_RED };
        Text::new_color(color, FT_SIZE_PT).draw(
                &"V",
                glyphs,
                &c.draw_state,
                transform,
                g
            ).unwrap();
        transform = transform.trans(16.0, 0.0);
        color = FT_COLOR_WHITE;
        Text::new_color(color, FT_SIZE_PT).draw(
                &"-",
                glyphs,
                &c.draw_state,
                transform,
                g
            ).unwrap();
        transform = transform.trans(16.0, 0.0);
        color = if cpu.get_flag(BREAK) == 1 { FT_COLOR_GREEN} else { FT_COLOR_RED };
        Text::new_color(color, FT_SIZE_PT).draw(
                &"B",
                glyphs,
                &c.draw_state,
                transform,
                g
            ).unwrap();
        transform = transform.trans(16.0, 0.0);
        color = if cpu.get_flag(DECIMAL) == 1 { FT_COLOR_GREEN} else { FT_COLOR_RED };
        Text::new_color(color, FT_SIZE_PT).draw(
                &"D",
                glyphs,
                &c.draw_state,
                transform,
                g
            ).unwrap();
        transform = transform.trans(16.0, 0.0);
        color = if cpu.get_flag(IRQ) == 1 { FT_COLOR_GREEN} else { FT_COLOR_RED };
        Text::new_color(color, FT_SIZE_PT).draw(
                &"I",
                glyphs,
                &c.draw_state,
                transform,
                g
            ).unwrap();
        transform = transform.trans(16.0, 0.0);
        color = if cpu.get_flag(ZERO) == 1 { FT_COLOR_GREEN} else { FT_COLOR_RED };
        Text::new_color(color, FT_SIZE_PT).draw(
                &"Z",
                glyphs,
                &c.draw_state,
                transform,
                g
            ).unwrap();
        transform = transform.trans(16.0, 0.0);
        color = if cpu.get_flag(CARRY) == 1 { FT_COLOR_GREEN} else { FT_COLOR_RED };
        Text::new_color(color, FT_SIZE_PT).draw(
                &"C",
                glyphs,
                &c.draw_state,
                transform,
                g
            ).unwrap();

        let cpu_register_texts = [
            &format!("A: {:#x} ({})", cpu.regs.a, cpu.regs.a),
            &format!("X: {:#x} ({})", cpu.regs.x, cpu.regs.y),
            &format!("Y: {:#x} ({})", cpu.regs.y, cpu.regs.x),
            &format!("Stack P: {:#x} ({})", cpu.regs.sp, cpu.regs.sp),
            &format!("Program P: {:#x}", cpu.regs.pc),
        ]; 

        for (i, &text) in cpu_register_texts.iter().enumerate() {
            let y_offset = (i+2) as f64 * (FT_LINE_DISTANCE + FT_SIZE_PX);
            let transform = c.transform.trans(offset[0], offset[1] + y_offset);
            Text::new_color(FT_COLOR_WHITE, FT_SIZE_PT).draw(
                &text,
                glyphs,
                &c.draw_state,
                transform,
                g
            ).unwrap();
        }
}

fn render_disasm(c: &Context, g: &mut G2d, glyphs: &mut Glyphs, disasm: &Disasm, pc: Addr, offset: [f64; 2]) {
    let mut transform = c.transform.trans(offset[0], offset[1]);
    for (text, addr) in disasm.instructions.iter().zip(disasm.addresses.iter()) {
        transform = transform.trans(0.0, FT_LINE_DISTANCE + FT_SIZE_PX);
        let color = if *addr == pc {
            FT_COLOR_RED
        } else {
            FT_COLOR_WHITE
        };
        Text::new_color(color, FT_SIZE_PT).draw(
            &text,
            glyphs,
             &c.draw_state,
            transform,
            g
        ).unwrap();
    }


}

fn render_memory(c: &Context, g: &mut G2d, glyphs: &mut Glyphs, bus: &MemoryBus, offset: [f64; 2]) {
    let mut transform_y = c.transform.trans(offset[0], offset[1]);
    for page in (0x0000..0x00F0).step_by(16) {
        transform_y = transform_y.trans(0.0, FT_LINE_DISTANCE+FT_SIZE_PX);
        Text::new_color(FT_COLOR_WHITE, FT_SIZE_PT).draw(
            &format!("{:#06x}:", page),
            glyphs,
            &c.draw_state,
            transform_y,
            g
        ).unwrap();
        let mut transform_x = transform_y.trans(40.0, 0.0);
        for offset in 0u16..16u16 {
            transform_x = transform_x.trans(20.0, 0.0);
            let addr = page + offset;
            Text::new_color(FT_COLOR_WHITE, FT_SIZE_PT).draw(
                &format!("{:02x}", bus.readb(addr)),
                glyphs,
                &c.draw_state,
                transform_x,
                g
            ).unwrap();
        }
    }

    transform_y = transform_y.trans(0.0, (FT_LINE_DISTANCE+FT_SIZE_PX) * 1.5);
    for page in (0x8000..0x80F0).step_by(16) {
        transform_y = transform_y.trans(0.0, (FT_LINE_DISTANCE+FT_SIZE_PX));
        Text::new_color(FT_COLOR_WHITE, FT_SIZE_PT).draw(
            &format!("{:#06x}:", page),
            glyphs,
            &c.draw_state,
            transform_y,
            g
        ).unwrap();
        let mut transform_x = transform_y.trans(40.0, 0.0);
        for offset in 0u16..16u16 {
            transform_x = transform_x.trans(20.0, 0.0);
            let addr = page + offset;
            Text::new_color(FT_COLOR_WHITE, FT_SIZE_PT).draw(
                &format!("{:02x}", bus.readb(addr)),
                glyphs,
                &c.draw_state,
                transform_x,
                g
            ).unwrap();
        }
    }
}