from cpu import CPU
from memory import Memory
from apu import APU
from ppu import PPU
from screen import Screen
from rom import ROM

class System:
    def __init__(self, rom_path):
        self.rom_path = rom_path
        self._screen = Screen()
        self._memory = Memory()
        self._ppu = PPU(self._screen, self._memory)
        self._apu = APU(self._memory)
        self._rom = ROM(rom_path)
        self._cpu = CPU(
            self._memory,
            self._ppu,
            self._apu,
            self._rom
        )

    def start(self):
        self._cpu.start()
    
    def reset(self):
        self._cpu.reset()