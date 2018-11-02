'''
Header format (16 bytes)
0-3: Constant $4E $45 $53 $1A ("NES" followed by MS-DOS end-of-file)
4: Size of PRG ROM in 16 KB units
5: Size of CHR ROM in 8 KB units (Value 0 means the board uses CHR RAM)
6: Flags 6
7: Flags 7
8: Size of PRG RAM in 8 KB units (Value 0 infers 8 KB for compatibility; see PRG RAM circuit)
9: Flags 9
10: Flags 10 (unofficial)
11-15: Zero filled
'''

class ROM(object):
    def __init__(self, path):
        self.path = path
        self.raw_rom = None
        self.header_size = 0x10
        self._read_rom_file()
        self._parse_rom()

    def _read_rom_file(self):
        with open(self.path, 'rb') as file:
            self.raw_rom = file.read()
    
    def _parse_rom(self):
        self.num_prg_blocks = self.raw_rom[4]
        # program data starts after header
        # and lasts for a set number of 16KB blocks
        end = self.header_size + ((16 * 1024) * self.num_prg_blocks)
        self.prg_bytes = self.raw_rom[self.header_size: end]