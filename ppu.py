class PPU:
    def __init__(self, screen, memory):
        self._screen = screen
        self._memory = memory
        self.start = 0x2000
        self.end = 0x2007