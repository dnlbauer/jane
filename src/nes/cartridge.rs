use crate::nes::mappers::*;
use crate::nes::bus::Memory;
use failure::Error;
use std::io::prelude::*;
use std::fs::File;
use crate::nes::types::*;
use std::path::Path;
use std::io::SeekFrom;

#[derive(Debug)]
struct Header {
    prg_rom_chunks: Byte,  // 16K chunks
    chr_rom_chunks: Byte,  // 8K chunks
    mapper1: Byte,
    mapper2: Byte,
    prg_ram_size: Byte,
    tv1: Byte,
    tv2: Byte
}

impl Header {
    fn new(f: &mut File) -> Result<Self, Error> {
        f.seek(SeekFrom::Start(0))?;

        // assert NES "NES"
        f.seek(SeekFrom::Start(4))?;

        let mut rom_sizes = [0; 2];  // prg size in 16K and chr size in 8K
        f.read_exact(&mut rom_sizes)?;

        let mut flags = [0; 5];
        f.read_exact(&mut flags)?;

        // Unused
        f.seek(SeekFrom::Current(5));

        Ok(Header {
            prg_rom_chunks: rom_sizes[0],
            chr_rom_chunks: rom_sizes[1],
            mapper1: flags[0],
            mapper2: flags[1],
            prg_ram_size: flags[2],
            tv1: flags[3],
            tv2: flags[4],
        })
    }

    pub fn has_trainer(&self) -> bool {
        self.mapper1 & (1 << 2) != 0
    }

    pub fn get_mapper_id(&self) -> Byte {
        let hi = (self.mapper2 >> 4) << 4;
        let lo = self.mapper1 >> 4;
        hi | lo
    }

}

pub struct Cartridge {
    prg_rom: Vec<Byte>,
    chr_rom: Vec<Byte>,
    mapper: Box<dyn Mapper>,
}

impl Cartridge {
    pub fn new(path: &Path) -> Result<Self, Error> {
        let mut f = File::open(path)?;
        let header = Header::new(&mut f)?;
        debug!("{:?}", header);

        let mut current_pos = 0;
        current_pos = f.seek(SeekFrom::Start(16))?;

        if header.has_trainer() {
            current_pos = f.seek(SeekFrom::Current(512))?;
        }

        let mut prg_rom = vec!(0; header.prg_rom_chunks as usize * 16384);
        f.read_exact(&mut prg_rom)?;
        let mut chr_rom = vec!(0; header.chr_rom_chunks as usize * 8192);
        f.read_exact(&mut chr_rom)?;
        

        let mapper = match header.get_mapper_id() {
            0 => { Mapper0::new(header.prg_rom_chunks, header.chr_rom_chunks) }
            id => bail!("Mapper {:04} not supported", id)
        };

        Ok(Cartridge {
            prg_rom: prg_rom,
            chr_rom: chr_rom,
            mapper: Box::new(mapper)
        })
    }

    pub fn dummy() -> Self {
        Cartridge {
            prg_rom: vec![0; 16384],
            chr_rom: vec![0; 8192],
            mapper: Box::new(Mapper0::new(1, 1))
        }
    }
}

impl Memory for Cartridge {
    fn readb(&self, addr: Addr) -> Byte {
        let mapped_addr = self.mapper.map_read_addr(addr);
        self.prg_rom[mapped_addr as usize]
    }

    fn writeb(&mut self, addr: Addr, data: Byte) {
        let mapped_addr = self.mapper.map_write_addr(addr);
        self.prg_rom[mapped_addr as usize] = data;
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_header_get_mapper_id() {
        let header = Header {
                prg_rom_chunks: 1,
                chr_rom_chunks: 1,
                mapper1: 0x00,
                mapper2: 0x00,
                prg_ram_size: 0x00,
                tv1: 0x00,
                tv2: 0x00
        };
        assert_eq!(0, header.get_mapper_id());

        let header = Header {
                prg_rom_chunks: 1,
                chr_rom_chunks: 1,
                mapper1: 0x10,
                mapper2: 0x00,
                prg_ram_size: 0x00,
                tv1: 0x00,
                tv2: 0x00
        };
        assert_eq!(1, header.get_mapper_id());


        let header = Header {
                prg_rom_chunks: 1,
                chr_rom_chunks: 1,
                mapper1: 0xff,
                mapper2: 0xff,
                prg_ram_size: 0x00,
                tv1: 0x00,
                tv2: 0x00
        };
        assert_eq!(255, header.get_mapper_id());
    }

    #[test]
    fn test_header_has_trainer() {
        let header = Header {
                prg_rom_chunks: 1,
                chr_rom_chunks: 1,
                mapper1: (1 << 2),
                mapper2: 0x00,
                prg_ram_size: 0x00,
                tv1: 0x00,
                tv2: 0x00
        };
        assert!(header.has_trainer());

        let header = Header {
                prg_rom_chunks: 1,
                chr_rom_chunks: 1,
                mapper1: 0x00,
                mapper2: 0x00,
                prg_ram_size: 0x00,
                tv1: 0x00,
                tv2: 0x00
        };
        assert!(!header.has_trainer());

        
    }

    #[test]
    fn test_cartridge_new() {
       let path = Path::new("test_roms/registers.nes");
       Cartridge::new(&path).unwrap();
    }

}