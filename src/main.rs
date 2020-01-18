#[macro_use] extern crate log;
#[macro_use] extern crate failure;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate bitflags;
extern crate image;
extern crate simple_logger;
extern crate phf;
extern crate piston_window;
extern crate rand;
extern crate fps_counter;

mod nes;

use std::env;
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
    simple_logger::init_with_level(Level::Debug).unwrap();
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        bail!("No cartridge supplied. Usage: ./jane cartridge.nes");
    } else {
        println!("Loading cartridge: {}", args[1]);
    }

    let mut nes = NES::new();
    let cartridge = Cartridge::new(Path::new(&args[1]))?;
    nes.insert_cartridge(cartridge);
    nes.start();
    if args.len() > 2 {
        let pc = Addr::from_str_radix(&args[2], 16)?;
        println!("Setting PC to {:#06x}", pc);
        nes.cpu.regs.pc = pc;
    }

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
    let mut run = false;
    while let Some(event) = events.next(&mut window) {
        if let Some(_) = event.render_args() {
            // Run enough clocks to render the next frame
            // if run { nes.clock_frame(); }
            if run { nes.clock_scanline(); }


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
                Key::L => { nes.clock_scanline() }
                Key::R => nes.reset(),
                Key::Space => run = !run,
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
        render_ppu(glyphs, &nes.ppu.borrow(), [debug_offset[0], debug_offset[1] + (25.0 * (FT_LINE_DISTANCE+FT_SIZE_PX))])
        // render_memory(glyphs, nes,
        //     [debug_offset[0] + 400.0, debug_offset[1]]);
    });
    glyphs.use_queue().draw(&mut window.encoder, &window.output_color).unwrap();
    window.encoder.flush(&mut window.device);

}

fn render_flags(glyphs: &mut GlyphBrush<Resources, Factory>, name: &str,
    names: Vec<&str>, values: Vec<bool>, offset: [f32; 2]) {
        let mut position = (offset[0], offset[1] + FT_SIZE_PX);
        glyphs.queue(Section {
            text: name,
            scale: *FT_SCALE,
            screen_position: position,
            color: FT_COLOR_WHITE,
            ..Section::default() 
        });

        position.0 += 70.0;
        for (flag, value) in names.iter().zip(values.iter()) {
            let color = if *value { FT_COLOR_GREEN } else { FT_COLOR_RED };
            glyphs.queue(Section {
                text: flag,
                scale: *FT_SCALE,
                screen_position: position,
                color: color,
                ..Section::default()
            });
            position.0 += 16.0;
        } 
}

fn render_cpu(glyphs: &mut GlyphBrush<Resources, Factory>, cpu: &CPU, offset: [f32; 2]) {
        render_flags(glyphs,
            "Flags",
            vec!["N", "V", "-", "B", "D", "I", "Z", "C"],
            vec![
                cpu.is_flag_set(Flags::NEGATIVE),
                cpu.is_flag_set(Flags::OVERFLOW),
                cpu.is_flag_set(Flags::UNUSED),
                cpu.is_flag_set(Flags::BREAK),
                cpu.is_flag_set(Flags::DECIMAL),
                cpu.is_flag_set(Flags::IRQ),
                cpu.is_flag_set(Flags::ZERO),
                cpu.is_flag_set(Flags::CARRY) 
            ],
            offset);

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

fn render_ppu(glyphs: &mut GlyphBrush<Resources, Factory>, ppu: &PPU, offset: [f32; 2]) {
    // draw position
    let mut position = [offset[0], offset[1] + FT_SIZE_PX];
    glyphs.queue(Section {
        text: &format!("SLine/Cycle: {:03}/{:03}", ppu.scanline, ppu.cycle),
        scale: *FT_SCALE,
        screen_position: (position[0], position[1]),
        color: FT_COLOR_WHITE,
        ..Section::default() 
    });

    // control register
    position[1] += FT_LINE_DISTANCE + FT_SIZE_PX;
    render_flags(glyphs,
            "Ctrl",
            vec!["V", "P", "H", "B", "S", "I", "N", "N"],
            vec![
                (ppu.regs.ctrl.bits() & (1 << 7)) > 0,
                (ppu.regs.ctrl.bits() & (1 << 6)) > 0,
                (ppu.regs.ctrl.bits() & (1 << 5)) > 0,
                (ppu.regs.ctrl.bits() & (1 << 4)) > 0,
                (ppu.regs.ctrl.bits() & (1 << 3)) > 0,
                (ppu.regs.ctrl.bits() & (1 << 2)) > 0,
                (ppu.regs.ctrl.bits() & (1 << 1)) > 0,
            ],
            position);

    // mask register
    position[1] += FT_LINE_DISTANCE + FT_SIZE_PX;
    render_flags(glyphs,
            "Mask",
            vec!["B", "G", "R", "s", "b", "M", "m", "G"],
            vec![
                (ppu.regs.mask.bits() & (1 << 7)) > 0,
                (ppu.regs.mask.bits() & (1 << 6)) > 0,
                (ppu.regs.mask.bits() & (1 << 5)) > 0,
                (ppu.regs.mask.bits() & (1 << 4)) > 0,
                (ppu.regs.mask.bits() & (1 << 3)) > 0,
                (ppu.regs.mask.bits() & (1 << 2)) > 0,
                (ppu.regs.mask.bits() & (1 << 1)) > 0,
            ],
            position);

    // status register
    position[1] += FT_LINE_DISTANCE + FT_SIZE_PX;
    render_flags(glyphs,
            "Status",
            vec!["V", "S", "O", "-", "-", "-", "-", "-"],
            vec![
                (ppu.regs.status.bits() & (1 << 7)) > 0,
                (ppu.regs.status.bits() & (1 << 6)) > 0,
                (ppu.regs.status.bits() & (1 << 5)) > 0,
                (ppu.regs.status.bits() & (1 << 4)) > 0,
                (ppu.regs.status.bits() & (1 << 3)) > 0,
                (ppu.regs.status.bits() & (1 << 2)) > 0,
                (ppu.regs.status.bits() & (1 << 1)) > 0,
            ],
            position);

    let ppu_register_texts = [
            &format!("OAM Addr: {0:#x}", ppu.regs.oam_addr),
            &format!("OAM Data: {0:#x}", ppu.regs.oam_data),
            &format!("Scroll: {0:#x}", ppu.regs.scroll),
            &format!("Addr: {0:#x}", ppu.regs.addr),
            &format!("Data: {:#x}", ppu.regs.data),
            &format!("DMA: {:#x}", ppu.regs.dma),
        ]; 

    position[1] += FT_LINE_DISTANCE + FT_SIZE_PX;
    for &text in ppu_register_texts.iter() {
        position[1] += FT_LINE_DISTANCE + FT_SIZE_PX;
        glyphs.queue(Section {
            text: text,
            scale: *FT_SCALE,
            screen_position: (position[0], position[1]),
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
    let lines_above_below: usize = 7;

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
