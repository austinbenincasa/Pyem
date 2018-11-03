'''
Memory Layout
$0000-$07FF	$0800	2KB internal RAM
$0800-$0FFF	$0800	Mirrors of $0000-$07FF
$1000-$17FF	$0800   Mirrors of $0000-$07FF
$1800-$1FFF	$0800   Mirrors of $0000-$07FF
$2000-$2007	$0008	NES PPU registers
$2008-$3FFF	$1FF8	Mirrors of $2000-2007 (repeats every 8 bytes)
$4000-$4017	$0018	NES APU and I/O registers
$4018-$401F	$0008	APU and I/O functionality that is normally disabled. See CPU Test Mode.
$4020-$FFFF	$BFE0	Cartridge space: PRG ROM, PRG RAM, and mapper registers (See Note)
'''

class Memory:
    def __init__(self):
        self._memory = [0]*65535

    def get_opcode(self, loc):
        '''
        Returns the opcode value of memory
        '''
        if self._memory[loc] == '':
            return 32
        else:
            return int(self._memory[loc], 16)

    def get_int(self, loc):
        '''
        Returns value of memory as int
        '''
        if self._memory[loc] == '':
            return 32
        else:
            return int(self._memory[loc], 16)

    def get(self, loc):
        '''
        Return value of memory as hex
        '''
        return self._memory[loc]
    
    def set(self, loc, value):
        '''
        Sets a location of memory as it
        '''
        self._memory[loc] = value

    def set_hex(self, loc, value):
        '''
        Sets a location of memory as hex
        '''
        val = int(value)
        if val > 0:
            self._memory[loc] = hex(int(value))[2:]
        else:
            self._memory[loc] = hex(int(value))[3:]
    
    def get_address(self):
        pass
    
    def len(self):
        return len(self._memory)
