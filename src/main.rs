#[macro_use] extern crate log;
#[macro_use] extern crate failure;
#[macro_use] extern crate lazy_static;
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
use failure::Error;

use gfx_glyph::{Section, GlyphBrushBuilder,GlyphBrush, Scale};
use gfx_device_gl::{Resources,Factory};

const BG_COLOR: [f32; 4] = [0.0, 0.0, 252.0/256.0, 1.0];

// font options
const FT_SIZE_PX: f32 = 11.0;
const FT_COLOR_WHITE: [f32; 4] = [1.0; 4];
const FT_COLOR_RED: [f32; 4] = [168.0/256.0, 32.0/256.0, 0.0, 1.0];
const FT_COLOR_GREEN: [f32; 4] = [0.0, 168.0/256.0, 0.0, 1.0];
const FT_LINE_DISTANCE: f32 = FT_SIZE_PX * 0.5;
lazy_static! {
    static ref FT_SCALE: Scale = Scale::uniform(FT_SIZE_PX);
}

fn main() -> Result<(), Error> {
    simple_logger::init_with_level(Level::Info).unwrap();
   
    let mut bus = MemoryBus::new();
    let cartridge = Path::new("test_roms/nestest.nes");
    let cartridge = Cartridge::new(cartridge)?;
    bus.insert_cartrige(cartridge);

    // Load little test program into ram
    // let test_prog: [Byte; 28] = [0xA2, 0x0A, 0x8E, 0x00, 0x00, 0xA2, 0x03, 0x8E, 0x01, 0x00, 0xAC, 0x00, 0x00, 0xA9,
    // 0x00, 0x18, 0x6D, 0x01, 0x00, 0x88, 0xD0, 0xFA, 0x8D, 0x02, 0x00, 0xEA, 0xEA, 0xEA];
    // let offset: Addr = 0x8000;
    // // craft test cartrige
    // let mut cart = Cartridge::dummy();
    // bus.insert_cartrige(cart);
    // for i in 0..test_prog.len() {
    //     let addr = (i as Addr) + offset;
    //     bus.writeb(addr, test_prog[i])
    // }
    // // hint program location to processor
    // bus.writew(0xfffc, offset);

    // disassemble instructions
    let disasm = Disasm::disassemble(&bus, 0xC000, 0xFFFF).unwrap();
    // println!("{:?}", disasm.instructions);
    // return;
    // let disasm = Disasm::disassemble(&[], 0x000).unwrap();

    // Create and reset CPU 
    let mut cpu: CPU = CPU::new();
    cpu.find_pc_addr(&bus);
    cpu.regs.pc = 0xC000;
    // return Ok(());
    // cpu.reset(&bus);

    // Prepare window and drawing resources
    let mut window: PistonWindow = WindowSettings::new("NESemu", [256*3, 240*2])
        .exit_on_esc(true).graphics_api(OpenGL::V3_2).build().unwrap();
    let mut event_settings = EventSettings::new();
    event_settings.max_fps = 60;
    event_settings.ups = 107400;
    let mut events = Events::new(event_settings);
    // let mut glyphs = get_glyphs(&mut window);
    let font: &[u8] = include_bytes!("../resources/fonts/PressStart2P.ttf");
    let mut glyph_brush: GlyphBrush<Resources, Factory> = GlyphBrushBuilder::using_font_bytes(font)
        .initial_cache_size((1024, 1024))
        .build(window.factory.clone());
    

    // Main loop
    let mut run = true;
    while let Some(event) = events.next(&mut window) {
        if let Some(_) = event.update_args() {
            // if cpu.regs.pc == 0xC6A2 { run = false }
            if run {
                cpu.clock(&mut bus);
            }
        }
        if let Some(_) = event.render_args() {
            render(&mut window, &event, &mut glyph_brush, &cpu, &bus, &disasm);
        }
        if let Some(Button::Keyboard(key)) = event.press_args() {
            match key {
                Key::C => cpu.clock(&mut bus),  // advance one clock
                Key::S => {  // advance to next instruction
                    cpu.clock(&mut bus);
                    while cpu.is_ahead() {
                        cpu.clock(&mut bus);
                    }
                }
                Key::R => cpu.reset(&mut bus),
                Key::Space => run = !run,
                Key::Up => {
                    event_settings.ups = (event_settings.ups as f64 * 1.1) as u64 + 1;
                    events = Events::new(event_settings);
                    debug!("UPS: {}", event_settings.ups);
                }
                Key::Down => {
                    event_settings.ups = (event_settings.ups as f64 * 0.91) as u64;
                    events = Events::new(event_settings);
                    debug!("UPS: {}", event_settings.ups);
                }
                _ => { }
            }
        }     
    }
    Ok(())
}

fn render(window: &mut PistonWindow, event: &Event,
    glyphs: &mut GlyphBrush<Resources, Factory>,
    cpu: &CPU, bus: &MemoryBus, disasm: &Disasm) {
    window.draw_2d(event, |c, g, d| {
        clear(BG_COLOR, g);


        let debug_offset = [10.0, 10.0];
        render_cpu(glyphs, cpu, debug_offset);
        render_disasm(glyphs, disasm, cpu.regs.pc,
            [debug_offset[0], debug_offset[1] + (7.0 * (FT_LINE_DISTANCE+FT_SIZE_PX))]);
        render_memory(glyphs, bus,
            [debug_offset[0] + 400.0, debug_offset[1]]);
        

    });
    glyphs.use_queue().draw(&mut window.encoder, &window.output_color).unwrap();
    window.encoder.flush(&mut window.device);
}

fn render_cpu(glyphs: &mut GlyphBrush<Resources, Factory>, cpu: &CPU, offset: [f32; 2]) {
        // draw flags
        let mut position = (offset[0], offset[1] + FT_SIZE_PX);
        glyphs.queue(Section {
            text: "Flags:",
            scale: *FT_SCALE,
            screen_position: position,
            color: FT_COLOR_WHITE,
            ..Section::default() 
        });

        position.0 += 70.0;
        let color = if cpu.get_flag(NEGATIVE) == 1 { FT_COLOR_GREEN } else { FT_COLOR_RED };
        glyphs.queue(Section {
                text: "N",
                scale: *FT_SCALE,
                screen_position: position,
                color: color,
                ..Section::default()
            });
        position.0 += 16.0;
        let color = if cpu.get_flag(OVERFLOW) == 1 { FT_COLOR_GREEN } else { FT_COLOR_RED };
        glyphs.queue(Section {
                text: "V",
                scale: *FT_SCALE,
                screen_position: position,
                color: color,
                ..Section::default()
            });
        position.0 += 16.0;
        let color = FT_COLOR_WHITE;
        glyphs.queue(Section {
                text: "-",
                scale: *FT_SCALE,
                screen_position: position,
                color: color,
                ..Section::default()
            });
        position.0 += 16.0;
        let color = if cpu.get_flag(BREAK) == 1 { FT_COLOR_GREEN } else { FT_COLOR_RED };
        glyphs.queue(Section {
                text: "B",
                scale: *FT_SCALE,
                screen_position: position,
                color: color,
                ..Section::default()
            });
        position.0 += 16.0;
        let color = if cpu.get_flag(DECIMAL) == 1 { FT_COLOR_GREEN } else { FT_COLOR_RED };
        glyphs.queue(Section {
                text: "D",
                scale: *FT_SCALE,
                screen_position: position,
                color: color,
                ..Section::default()
            });
        position.0 += 16.0;
        let color = if cpu.get_flag(IRQ) == 1 { FT_COLOR_GREEN } else { FT_COLOR_RED };
        glyphs.queue(Section {
                text: "I",
                scale: *FT_SCALE,
                screen_position: position,
                color: color,
                ..Section::default()
            });
        position.0 += 16.0;
        let color = if cpu.get_flag(ZERO) == 1 { FT_COLOR_GREEN } else { FT_COLOR_RED };
        glyphs.queue(Section {
                text: "Z",
                scale: *FT_SCALE,
                screen_position: position,
                color: color,
                ..Section::default()
            });
        position.0 += 16.0;
        let color = if cpu.get_flag(CARRY) == 1 { FT_COLOR_GREEN } else { FT_COLOR_RED };
        glyphs.queue(Section {
                text: "C",
                scale: *FT_SCALE,
                screen_position: position,
                color: color,
                ..Section::default()
            });
        position.0 += 16.0;
        let color = FT_COLOR_WHITE;
        glyphs.queue(Section {
                text: &format!("{:#06x}", cpu.regs.flags),
                scale: *FT_SCALE,
                screen_position: position,
                color: color,
                ..Section::default()
            });

        let cpu_register_texts = [
            &format!("A: {0:#x} ({0})", cpu.regs.a),
            &format!("X: {0:#x} ({0})", cpu.regs.x),
            &format!("Y: {0:#x} ({0})", cpu.regs.y),
            &format!("Stack P: {0:#x} ({0})", cpu.regs.sp),
            &format!("Program P: {:#x}", cpu.regs.pc),
        ]; 

        for (i, &text) in cpu_register_texts.iter().enumerate() {
            let y_offset = (i+2) as f32 * (FT_LINE_DISTANCE + FT_SIZE_PX);
            let position = (offset[0], offset[1] + y_offset);
            glyphs.queue(Section {
                text: text,
                scale: *FT_SCALE,
                screen_position: position,
                color: FT_COLOR_WHITE,
                ..Section::default()
            });
        }
}

fn render_disasm(glyphs: &mut GlyphBrush<Resources, Factory>,
    disasm: &Disasm, pc: Addr, offset: [f32; 2]) {
    let pc_position = disasm.addresses.iter().position(|&pos| pos == pc);
    if let None = pc_position {
        return;
    }
    let pc_position = pc_position.unwrap();
    let lines_above_below: usize = 12;

    let start = if (pc_position as i32 - lines_above_below as i32) < 0 {
         0
     } else {
        pc_position - lines_above_below
    };

    let text_position_x = offset[0];
    let mut text_position_y = offset[1]; 
    for i in (start..disasm.addresses.len()).take(lines_above_below*2+1) {
        let txt = &disasm.instructions[i];
        text_position_y += FT_LINE_DISTANCE + FT_SIZE_PX;
        let color = if i == pc_position {
            FT_COLOR_RED
        } else {
            FT_COLOR_WHITE
        };
        glyphs.queue(Section {
            text: txt,
            scale: *FT_SCALE,
            screen_position: (text_position_x, text_position_y),
            color: color,
            ..Section::default() 
        });
    }
}

fn render_memory(glyphs: &mut GlyphBrush<Resources, Factory>, bus: &MemoryBus, offset: [f32; 2]) {
    let mut position_y = (offset[0], offset[1]);
    for page in (0x0000..0x00FF).step_by(16) {
        position_y.1 += FT_LINE_DISTANCE + FT_SIZE_PX;
        let mut line = format!("{:#06x}:", page);
        (0u16..16u16).map(|offset| offset + page)
            .map(|addr| bus.readb(addr))
            .map(|val| format!(" {:02x}", val))
            .for_each(|s| line.push_str(&s));
        glyphs.queue(Section {
            text: &line,
            scale: *FT_SCALE,
            screen_position: position_y,
            color: FT_COLOR_WHITE,
            ..Section::default()
        });
    }

    position_y.1 += FT_LINE_DISTANCE+FT_SIZE_PX * 1.5;
    for page in (0x0400..0x06FF).step_by(16) {
        position_y.1 += FT_LINE_DISTANCE + FT_SIZE_PX;
        let mut line = format!("{:#06x}:", page);
        (0u16..16u16).map(|offset| offset + page)
            .map(|addr| bus.readb(addr))
            .map(|val| format!(" {:02x}", val))
            .for_each(|s| line.push_str(&s));
        glyphs.queue(Section {
            text: &line,
            scale: *FT_SCALE,
            screen_position: position_y,
            color: FT_COLOR_WHITE,
            ..Section::default()
        });
    }
}