#[macro_use] extern crate log;
#[macro_use] extern crate failure;
#[macro_use] extern crate lazy_static;
extern crate image;
extern crate simple_logger;
extern crate phf;
extern crate piston_window;
extern crate rand;
extern crate fps_counter;

mod nes;

use crate::nes::*;
use std::path::Path;
use piston_window::*;
use nes::cpu::*;
use nes::disasm::*;
use opengl_graphics::OpenGL;
use log::Level;
use failure::Error;
use fps_counter::FPSCounter;
use gfx_glyph::{Section, GlyphBrushBuilder,GlyphBrush, Scale};
use gfx_device_gl::{Resources,Factory};

const BG_COLOR: [f32; 4] = [0.0, 0.0, 0.0, 1.0];

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
    simple_logger::init_with_level(Level::Warn).unwrap();
  
    let mut nes = NES::new();
    let cartridge = Path::new("test_roms/nestest.nes");
    let cartridge = Cartridge::new(cartridge)?;
    nes.insert_cartridge(cartridge);
    nes.start();
    // nes.cpu.regs.pc = 0xC000;

    // disassemble instructions
    let disasm = Disasm::disassemble(&nes.memory, 0xC000, 0xFFFF).unwrap();

    // Prepare window and drawing resources

    // debugger + scaled nes resolution + border
    let window_width = 300 + 256 * 2 + 5;
    let window_height = 40 + 240*2 + 5;
    let mut window: PistonWindow = WindowSettings::new("xXx NESemu xXx", [window_width, window_height])
        .exit_on_esc(true).graphics_api(OpenGL::V3_2).build().unwrap();
    let mut event_settings = EventSettings::new();
    event_settings.max_fps = 60;
    let mut events = Events::new(event_settings);

    // font for debug screens
    let font: &[u8] = include_bytes!("../resources/fonts/PressStart2P.ttf");
    let mut glyphs: GlyphBrush<Resources, Factory> = GlyphBrushBuilder::using_font_bytes(font)
        .initial_cache_size((1024, 1024))
        .build(window.factory.clone());

    // fps counter
    let mut fps = FPSCounter::new();
    
    // main screen texture
    let mut texture_ctx = TextureContext {
        factory: window.factory.clone(),
        encoder: window.factory.create_command_buffer().into(),
    }; 
    let mut texture: G2dTexture = Texture::from_image(
        &mut texture_ctx,
        &nes.ppu.borrow().canvas_main,
        &TextureSettings::new()
    ).unwrap();
    
    
    // Main loop
    let mut run = true;
    while let Some(event) = events.next(&mut window) {
        if let Some(_) = event.render_args() {
            // Run enough clocks to render the next frame
            if run { nes.clock_frame(); }

            texture.update(&mut texture_ctx, &nes.ppu.borrow().canvas_main).unwrap();
            window.draw_2d(&event, |c, g, d| {
                clear(BG_COLOR, g);
                texture_ctx.encoder.flush(d);
                let transform = c.transform.trans(300.0, 40.0).scale(2.0, 2.0);
                image(&texture, transform, g);

                // fps
                let fps = fps.tick();
                let position = (410.0, 10.0 + FT_SIZE_PX);
                let color = if fps < 60 { FT_COLOR_RED } else { FT_COLOR_WHITE };
                glyphs.queue(Section {
                    text: &format!("FPS: {}", fps),
                    scale: *FT_SCALE,
                    screen_position: position,
                    color: color,
                    ..Section::default() 
                });

            });
            render_debug(&mut window, &event, &mut glyphs, &nes, &disasm);
        }
        if let Some(Button::Keyboard(key)) = event.press_args() {
            match key {
                Key::C => nes.clock(),  // advance one clock
                Key::S => { nes.clock_instruction() }
                Key::F => { nes.clock_frame() }
                Key::R => nes.reset(),
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

fn render_debug(window: &mut PistonWindow, event: &Event,
    glyphs: &mut GlyphBrush<Resources, Factory>,
    nes: &NES, disasm: &Disasm) {

    
    window.draw_2d(event, |_c, _g, _d| {
        let debug_offset = [10.0, 10.0];
        render_cpu(glyphs, &nes.cpu, debug_offset);
        render_disasm(glyphs, disasm, nes.cpu.regs.pc,
            [debug_offset[0], debug_offset[1] + (8.0 * (FT_LINE_DISTANCE+FT_SIZE_PX))]);
        // render_memory(glyphs, nes,
        //     [debug_offset[0] + 400.0, debug_offset[1]]);
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
            &format!("#CPU Cycles: {}", cpu.cycles),
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

fn render_memory(glyphs: &mut GlyphBrush<Resources, Factory>, nes: &NES, offset: [f32; 2]) {
    let mut position_y = (offset[0], offset[1]);
    for page in (0x0000..0x00FF).step_by(16) {
        position_y.1 += FT_LINE_DISTANCE + FT_SIZE_PX;
        let mut line = format!("{:#06x}:", page);
        (0u16..16u16).map(|offset| offset + page)
            .map(|addr| nes.memory.readb(addr))
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
            .map(|addr| nes.memory.readb(addr))
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
