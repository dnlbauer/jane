# Jane - Just another NES emulator

Just another implementation of a NES emulator written in Rust.

**A note of warning:** This is an exploratory coding project with the aim to understand concepts of assembler and emulation in general. If you are looking for a cycle-accurate ready-to-use NES emulator to play some games, this is the wrong one.

Usage:
``` bash
./jane super_mario.nes

# With optional start address for the CPU (mainly for debugging)
./jane nestest.nes C000
```

Make sure to compile with `--release` for 60 fps.

## what works
* CPU
* Reading Roms (iNES) 
* Memory mapping and RAM
* A very simplistic debugger

## what does not work
* GPU / graphics
* APU / sound
* Controllers
* a lot of mappers
* game saves
* everything else
