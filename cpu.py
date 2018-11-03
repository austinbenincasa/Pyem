class CPU:
    '''
    Emulates the 6502 processor
    '''
    def __init__(self, memory, ppu, apu, rom):
        self._a = 0 # accumulator register
        self._x = 0 # x register
        self._y = 0 # y register
        self._sp = 0xFD # stack pointer
        self._p = 0b00000000 #(NVss-DIZC)
        self._pc = 0 # program counter
        self._rom = rom
        self._apu = apu
        self._ppu = ppu
        self._memory = memory
        self._running = False
        self._interupted = False
        self._address_modes = [
            ('imp', 1, ''), #(MODE, BYTES, FMT)
            ('acc', 1, 'A'),
            ('imm', 2, '#$00'),
            ('zp', 2, '$00'),
            ('zpx', 2, '$00,X'),
            ('zpy', 2, '$00,Y'),
            ('izx', 2, '($00,X)'),
            ('izy', 2, '($00),Y'),
            ('abs', 3, '$0000'),
            ('abx', 3, '$0000,X'),
            ('aby', 3, '$0000,Y'),
            ('ind', 3, '($0000)'),
            ('rel', 2, '$0000'),
        ]
        self._opcodes = [
            ('BRK', 0, 7), #(CMD, MODE, CYCLES)
            ('ORA', 6, 6),
            ('KIL', 0, 0),
            ('SLO', 6, 8),
            ('NOP', 3, 3),
            ('ORA', 3, 3),
            ('ASL', 3, 5),
            ('SLO', 3, 5),
            ('PHP', 0, 3),
            ('ORA', 2, 2),
            ('ASL', 0, 2),
            ('ANC', 2, 2),
            ('NOP', 8, 4),
            ('ORA', 8, 4),
            ('ASL', 8, 6),
            ('SLO', 8, 6),
            ('BPL', 12, 2),
            ('ORA', 7, 5),
            ('KIL', 0, 0),
            ('SLO', 7, 8),
            ('NOP', 4, 4),
            ('ORA', 4, 4),
            ('ASL', 4, 6),
            ('SLO', 4, 6),
            ('CLC', 0, 2),
            ('ORA', 10, 4),
            ('NOP', 0, 2),
            ('SLO', 10, 7),
            ('NOP', 9, 4),
            ('ORA', 9, 4),
            ('ASL', 9, 7),
            ('SLO', 9, 7),
            ('JSR', 8, 6),
            ('AND', 6, 6),
            ('KIL', 0, 0),
            ('RLA', 6, 8),
            ('BIT', 3, 3),
            ('AND', 3, 3),
            ('ROL', 3, 5),
            ('RLA', 3, 5),
            ('PLP', 0, 4),
            ('AND', 2, 2),
            ('ROL', 1, 2),
            ('ANC', 2, 2),
            ('BIT', 8, 4),
            ('AND', 8, 4),
            ('ROL', 8, 6),
            ('RLA', 8, 6),
            ('BMI', 12, 2),
            ('AND', 7, 5),
            ('KIL', 0, 0),
            ('RLA', 7, 8),
            ('NOP', 4, 4),
            ('AND', 4, 4),
            ('ROL', 4, 6),
            ('RLA', 4, 6),
            ('SEC', 0, 2),
            ('AND', 10, 4),
            ('NOP', 0, 2),
            ('RLA', 10, 7),
            ('NOP', 9, 4),
            ('AND', 9, 4),
            ('ROL', 9, 7),
            ('RLA', 9, 7),
            ('RTI', 0, 6),
            ('EOR', 6, 6),
            ('KIL', 0, 0),
            ('SRE', 6, 8),
            ('NOP', 3, 3),
            ('EOR', 3, 3),
            ('LSR', 3, 5),
            ('SRE', 3, 5),
            ('PHA', 0, 3),
            ('EOR', 2, 2),
            ('LSR', 1, 2),
            ('ALR', 2, 2),
            ('JMP', 8, 3),
            ('EOR', 8, 4),
            ('LSR', 8, 6),
            ('SRE', 8, 6),
            ('BVC', 12, 2),
            ('EOR', 7, 5),
            ('KIL', 0, 0),
            ('SRE', 7, 8),
            ('NOP', 4, 4),
            ('EOR', 4, 4),
            ('LSR', 4, 6),
            ('SRE', 4, 6),
            ('CLI', 0, 2),	
            ('EOR', 10, 4),
            ('NOP', 0, 2),
            ('SRE', 10, 7),
            ('NOP', 9, 4),
            ('EOR', 9, 4),
            ('LSR', 9, 7),
            ('SRE', 9, 7),
            ('RTS', 0, 6),
            ('ADC', 6, 6),
            ('KIL', 0, 0),
            ('RRA', 6, 8),
            ('NOP', 3, 3),
            ('ADC', 3, 3),
            ('ROR', 3, 5),
            ('RRA', 3, 5),
            ('PLA', 0, 4),
            ('ADC', 2, 2),
            ('ROR', 1, 2),
            ('ARR', 2, 2),
            ('JMP', 11, 5),
            ('ADC', 8, 4),
            ('ROR', 8, 6),
            ('RRA', 8, 6),
            ('BVS', 12, 2),
            ('ADC', 7, 5),
            ('KIL', 0, 0),
            ('RRA', 7, 8),
            ('NOP', 4, 4),
            ('ADC', 4, 4),
            ('ROR', 4, 6),
            ('RRA', 4, 6),
            ('SEI', 0, 1), #Cycle should be 1? not 2
            ('ADC', 10, 4),
            ('NOP', 0, 2),
            ('RRA', 10, 7),
            ('NOP', 9, 4),
            ('ADC', 9, 4),
            ('ROR', 9, 7),
            ('RRA', 9, 7),
            ('NOP', 2, 2),
            ('STA', 6, 6),
            ('NOP', 2, 2),
            ('SAX', 6, 6),
            ('STY', 3, 3),
            ('STA', 3, 3),
            ('STX', 3, 3),
            ('SAX', 3, 3),
            ('DEY', 0, 2),
            ('NOP', 2, 2),
            ('TXA', 0, 2),
            ('XAA', 2, 2),
            ('STY', 8, 4),
            ('STA', 8, 4),
            ('STX', 8, 4),
            ('SAX', 8, 4),
            ('BCC', 12, 2),
            ('STA', 7, 6),
            ('KIL', 0, 0),
            ('AHX', 7, 6),
            ('STY', 4, 4),
            ('STA', 4, 4),
            ('STX', 5, 4),
            ('SAX', 5, 4),
            ('TYA', 0, 2),
            ('STA', 10, 5),
            ('TXS', 0, 2),
            ('TAS', 10, 5),
            ('SHY', 9, 5),
            ('STA', 9, 5),
            ('SHX', 10, 5),
            ('AHX', 10, 5),
            ('LDY', 2, 2),
            ('LDA', 6, 6),
            ('LDX', 2, 2),
            ('LAX', 6, 6),
            ('LDY', 3, 3),
            ('LDA', 3, 3),
            ('LDX', 3, 3),
            ('LAX', 3, 3),
            ('TAY', 0, 2),
            ('LDA', 2, 2),
            ('TAX', 0, 2),
            ('LAX', 2, 2),
            ('LDY', 8, 4),
            ('LDA', 8, 4),
            ('LDX', 8, 4),
            ('LAX', 8, 4),
            ('BCS', 12, 2),
            ('LDA', 7, 5),
            ('KIL', 0, 0),
            ('LAX', 7, 5),
            ('LDY', 4, 4),
            ('LDA', 4, 4),
            ('LDX', 5, 4),
            ('LAX', 5, 4),
            ('CLV', 0, 2),
            ('LDA', 10, 4),
            ('TSX', 0, 2),
            ('LAS', 10, 4),
            ('LDY', 9, 4),
            ('LDA', 9, 4),
            ('LDX', 10, 4),
            ('LAX', 10, 4),
            ('CPY', 2, 2),
            ('CMP', 6, 6),
            ('NOP', 2, 2),
            ('DCP', 6, 8),
            ('CPY', 3, 3),
            ('CMP', 3, 3),
            ('DEC', 3, 5),
            ('DCP', 3, 5),
            ('INY', 0, 2),
            ('CMP', 2, 2),
            ('DEX', 0, 2),
            ('AXS', 2, 2),
            ('CPY', 8, 4),
            ('CMP', 8, 4),
            ('DEC', 8, 6),
            ('DCP', 8, 6),
            ('BNE', 12, 2),
            ('CMP', 7, 5),
            ('KIL', 0, 0),
            ('DCP', 7, 8),
            ('NOP', 4, 4),
            ('CMP', 4, 4),
            ('DEC', 4, 6),
            ('DCP', 4, 6),
            ('CLD', 0, 2),
            ('CMP', 10, 4),
            ('NOP', 0, 2),
            ('DCP', 10, 7),
            ('NOP', 9, 4),
            ('CMP', 9, 4),
            ('DEC', 9, 7),
            ('DCP', 9, 7),
            ('CPX', 2, 2),
            ('SBC', 6, 6),
            ('NOP', 2, 2),
            ('ISC', 6, 8),
            ('CPX', 3, 3),
            ('SBC', 3, 3),
            ('INC', 3, 5),
            ('ISC', 3, 5),
            ('INX', 0, 2),
            ('SBC', 2, 2),
            ('NOP', 0, 2),
            ('SBC', 2, 2),
            ('CPX', 8, 4),
            ('SBC', 8, 4),
            ('INC', 8, 6),
            ('ISC', 8, 6),
            ('BEQ', 12, 2),
            ('SBC', 7, 5),
            ('KIL', 0, 0),
            ('ISC', 7, 8),
            ('NOP', 4, 4),
            ('SBC', 4, 4),
            ('INC', 4, 6),
            ('ISC', 4, 6),
            ('SED', 0, 2),
            ('SBC', 10, 4),
            ('NOP', 0, 2),
            ('ISC', 10, 7),
            ('NOP', 9, 4),
            ('SBC', 9, 4),
            ('INC', 9, 7),
            ('ISC', 9, 7),
        ]
    def start(self):
        self._boot()
        while(self._running):
            while(not self._interupted):
                self._execute_opcode(
                    self._memory.get(self._pc),
                    self._memory.get_opcode(self._pc)
                )
            while(self._interupted):
                pass
            '''
            executeCPU(cycles_to_execute);
            generateInterrupts();
            emulateGraphics();
            emulateSound();
            emulateOtherSoftware();
            timeSincronization();
            '''

    def _boot(self):
        '''
        Inits all the system components
        '''
        self._load_rom()
        self._running = True


    def _load_rom(self):
        '''
        Load ROM into memory
        '''
        for i, byte in enumerate(self._rom.prg_bytes):
            self._memory.set_hex(
                i, 
                byte
            )

    def reset(self):
        '''
        A, X, Y were not affected
        S was decremented by 3 (but nothing was written to the stack)
        The I (IRQ disable) flag was set to true (status ORed with $04)
        The internal memory was unchanged
        APU mode in $4017 was unchanged
        APU was silenced ($4015 = 0)
        '''
        pass

    def _system_dump(self, cur_cmd, mode, args, error):
        '''
        Dump system information for debugging
        purposes
        '''
        print(f"\n\nException occured: {error} \nSystem Haulted!\n")
        print(f"Instr: {cur_cmd}")
        print(f"Adr Mode: {mode}")
        print(f"A reg: {self._a}")
        print(f"X reg: {self._x}")
        print(f"Y reg: {self._y}")
        print(f"SP reg: {self._sp}")
        print(f"P reg: {self._p}")
        print(f"PC reg: {self._pc}")
        exit("\n")

    def _get_mode(self, index):
        '''
        Get addressing mode of opcode
        '''
        return self._address_modes[index]

    def _execute_opcode(self, opcode: bytes, op_num: int) -> None:
        '''
        Method for executing a opcode
        '''
        entry = self._opcodes[op_num]
        func = entry[0]
        mode = self._address_modes[entry[1]] #for debug
        print(f'{opcode}: {op_num} -> {func} {mode[2]}')
        getattr(self, f'_{func}')(
            entry[1], 
            mode[1],
            entry[2]
        )
    
    def _get_address(self, mode):
        '''
        Determine memory address given the adressing mode
        '''
        if mode == 0: # implicit
            return 0
        elif mode == 1: # accumulator
            return 0
        elif mode == 2: # immediate
            return int(self._pc + 1)
        elif mode == 3: # zero page
            l = self._memory.get(self._pc + 1)
            return int('00' + l, 16)
        elif mode == 4: # zero Page,x
            zp = self._memory.get(self._pc + 1)
            adr = int(zp) + self._x
            if adr <= 255:
                return adr
            else:
                return (adr-255)
        elif mode == 5: # zero page,y
            zp = self._memory.get(self._pc + 1)
            adr = int(zp) + self._y
            if adr <= 255:
                return adr
            else:
                return (adr-255)
        elif mode == 6: # Indexed Indirect
            ip = self._memory.get(self._pc + 1)
            adr = int(ip) + self._x
            if adr <= 255:
                return adr
            else:
                return (adr-255)
        elif mode == 7: # Indirect Indexed
            l = self._memory.get(self._pc + 1)
            return int('00' + l) + self._y
        elif mode == 8: # absolute
            m = self._memory.get(self._pc + 2)
            l = self._memory.get(self._pc + 1)
            return int(m + l, 16)
        elif mode == 9: # absolute,X
            m = self._memory.get(self._pc + 2)
            l = self._memory.get(self._pc + 1)
            return int(m + l, 16) + self._x
        elif mode == 10: # absolute,Y
            m = self._memory.get(self._pc + 2)
            l = self._memory.get(self._pc + 1)
            return int(m + l, 16) + self._y
        elif mode == 11: #indirect
            m = self._memory.get(self._pc + 2)
            l = self._memory.get(self._pc + 1)
            return int(m + l, 16) + self._x
        elif mode == 12: #relative
            l = self._memory.get(self._pc + 1)
            return self._pc + int(l, 16)

    def _ADC(self, mode, args, cycles):
        '''
        Add with Carry
        A:=A+{adr}
        modes 2,3,4,6,7,8,9,10
        '''
        value = 0
        adr = self._get_address(mode)
        self._a += value
        #self._system_dump('ADC', mode, args, "Invalid mode number")
        
        self._pc += args


    def _AND(self, mode, args, cycles):
        '''
        Logical AND
        A:=A&{adr}
        modes 2,3,4,6,7,8,9,10
        '''
        adr = self._get_address(mode)
        self._a = self._a & self._memory.get_int(adr)
        #self._system_dump('AND', mode, args, "Invalid mode number")
        self._pc += args

    def _ASL(self, mode, args, cycles):
        '''
        Arithmetic Shift left
        {adr}:={adr}*2
        modes 1,3,4,8,9
        '''
        val = 0
        adr = self._get_address(mode)
        #self._system_dump('ASL', mode, args, "Invalid mode number")
        self._pc += args

    def _BCC(self, mode, args, cycles):
        '''
        Branch if carry flag clear
        branch on C=0
        modes 12
        '''
        self._pc += args
        if not self._p & (1 << 1):
            self._pc = self._get_address(mode)
            # +1 cycle if branch succeds
            # +2 cycle if to a new page
        #self._system_dump('BCC', mode, args, "Invalid mode number")

    def _BCS(self, mode, args, cycles):
        '''
        Branch if carry flag set
        branch on C=1
        modes 12
        '''
        self._pc += args
        if self._p & (1 << 0):
            self._pc = self._get_address(mode)
            # +1 cycle if branch succeds
            # +2 cycle if to a new page
        #self._system_dump('BCS', mode, args, "Invalid mode number")

    def _BEQ(self, mode, args, cycles):
        '''
        Branch if zero flag set
        branch on Z=1
        modes 12
        '''
        self._pc += args
        if self._p & (1 << 1):
            self._pc = self._get_address(mode)
            # +1 cycle if branch succeds
            # +2 cycle if to a new page
        #self._system_dump('BEQ', mode, args, "Invalid mode number")

    def _BIT(self, mode, args, cycles):
        '''
        Bit Test
        N:=b7 V:=b6 Z:=A&{adr}
        '''
        adr = self._get_address(mode)
        if mode == 3:
            l = self._memory.get(self._pc + 1)
            adr = int('00' + l, 16)
        elif mode == 8:
            m = self._memory.get(self._pc + 2)
            l = self._memory.get(self._pc + 1)
        else:
            self._system_dump('BIT', mode, args, "Invalid mode number")
        self._pc += args

    def _BMI(self, mode, args, cycles):
        '''
        Branch if negative flag set
        branch on N=1
        '''
        self._pc += args
        if self._p & (1 << 7):
            self._pc = self._get_address(mode)
            # +1 cycle if branch succeds
            # +2 cycle if to a new page
        #self._system_dump('BMI', mode, args, "Invalid mode number")

    def _BNE(self, mode, args, cycles):
        '''
        Branch if zero flag clear
        branch on Z=0
        modes 12
        '''
        self._pc += args
        if not self._p & (1 << 1):
            self._pc = self._get_address(mode)
            # +1 cycle if branch succeds
            # +2 cycle if to a new page
        #self._system_dump('BNE', mode, args, "Invalid mode number")

    def _BPL(self, mode, args, cycles):
        '''
        Branch if negative flag clear
        branch on N=0
        modes 12
        '''
        self._pc += args
        if not self._p & (1 << 7):
            self._pc = self._get_address(mode)
            # +1 cycle if branch succeds
            # +2 cycle if to a new page
        # self._system_dump('BPL', mode, args, "Invalid mode number")

    def _BRK(self, mode, args, cycles):
        '''
        Force an interrupt
        (S)-:=PC,P PC:=($FFFE)
        modes 0
        '''
        self._sp -= 1
        self._memory.set_hex(self._sp, self._pc)
        self._pc = self._memory.get_int()
        #self._system_dump('BRK', mode, args, "Invalid mode number")
        self._pc += args

    def _BVC(self, mode, args, cycles):
        '''
        Branch if overflow flag clear
        branch on V=0
        modes 12
        '''
        self._pc += args
        if not self._p & (1 << 6):
            self._pc = self._get_address(mode)
            # +1 cycle if branch succeds
            # +2 cycle if to a new page
        #self._system_dump('BVC', mode, args, "Invalid mode number")

    def _BVS(self, mode, args, cycles):
        '''
        Branch if overflow flag set
        branch on V=1
        modes 12
        '''
        self._pc += args
        if self._p & (1 << 6):
            self._pc = self._get_address(mode)
            # +1 cycle if branch succeds
            # +2 cycle if to a new page
        #self._system_dump('BVS', mode, args, "Invalid mode number")

    def _CLC(self, mode, args, cycles):
        '''
        Clear carry flag
        C:=0
        modes 0
        '''
        self._p = self._p & ~( 1 << 0)
        #self._system_dump('CLC', mode, args, "Invalid mode number")
        self._pc += args

    def _CLD(self, mode, args, cycles):
        '''
        Clear decimal mode flag
        D:=0
        modes 0
        '''
        self._p = self._p & ~( 1 << 3)
        #self._system_dump('CLD', mode, args, "Invalid mode number")
        self._pc += args

    def _CLI(self, mode, args, cycles):
        '''
        Clear interrupt disable flag
        I:=0
        '''
        self._p = self._p & ~( 1 << 2)
        #self._system_dump('CLI', mode, args, "Invalid mode number")
        self._pc += args

    def _CLV(self, mode, args, cycles):
        '''
        Clear overflow flag
        V:=0
        modes 0
        '''
        self._p = self._p & ~( 1 << 6)
        #self._system_dump('CLV', mode, args, "Invalid mode number")
        self._pc += args

    def _CMP(self, mode, args, cycles):
        '''
        Compare accumulator
        A-{adr}
        modes 2,3,4,6,7,8,9,10
        '''
        value = 0
        adr = self._get_address(mode)
        value = self._memory.get_int(adr)
        #self._system_dump('CMP', mode, args, "Invalid mode number")

        if self._a >= value:
            self._p = self._p & ( 1 << 0)
        if self._a == value:
            self._p = self._p & ( 1 << 1)
        self._pc += args

    def _CPX(self, mode, args, cycles):
        '''
        Compare X register
        X-{adr}
        modes 2,3,8
        '''
        value = 0
        adr = self._get_address(mode)
        value = self._memory.get_int(adr)
        #self._system_dump('CPX', mode, args, "Invalid mode number")

        if self._x >= value:
            self._p = self._p & ( 1 << 0)
        if self._x == value:
            self._p = self._p & ( 1 << 1)
        self._pc += args

    def _CPY(self, mode, args, cycles):
        '''
        Compare Y register
        Y-{adr}
        modes 2,3,8
        '''
        value = 0
        adr = self._get_address(mode)
        value = self._memory.get_int(adr)
        #self._system_dump('CPY', mode, args, "Invalid mode number")

        if self._y >= value:
            self._p = self._p & ( 1 << 0)
        if self._y == value:
            self._p = self._p & ( 1 << 1)
        self._pc += args

    def _DEC(self, mode, args, cycles):
        '''
        Decrement a memory location
        {adr}:={adr}-1
        modes 3,4,8,9
        '''
        adr = self._get_address(mode)
        self._memory.set_hex(
            adr,
            self._memory.get_int(adr) - 1
        )
        #self._system_dump('DEC', mode, args, "Invalid mode number")
        
        if adr == 0:
            self._p = self._p & ( 1 << 1)

        self._pc += args

    def _DEX(self, mode, args, cycles):
        '''
        Decrement the X register
        X:=X-1
        '''
        if mode == 0:
            self._x = self._x - 1
        else:
            self._system_dump('DEX', mode, args, "Invalid mode number")

        if self._x == 0:
            self._p = self._p & ( 1 << 1)

        self._pc += args

    def _DEY(self, mode, args, cycles):
        '''
        Decrement the Y register
        Y:=Y-1
        '''
        if mode == 0:
            self._y = self._y - 1
        else:
            self._system_dump('DEY', mode, args, "Invalid mode number")
        
        if self._y == 0:
            self._p = self._p & ( 1 << 1)

        self._pc += args

    def _EOR(self, mode, args, cycles): #IDK
        '''
        Exclusive OR
        A:=A exor {adr}
        modes 2,3,4,6,7,8,9,10
        '''
        adr = self._get_address(mode)
        self._a = self._a | self._memory.get_int(adr)
        #self._system_dump('EOR', mode, args, "Invalid mode number")

        if self._a == 0:
            self._p = self._p & ( 1 << 1)

        self._pc += args

    def _INC(self, mode, args, cycles):
        '''
        Increment a memory location
        {adr}:={adr}+1
        modes 3,4,8,9
        '''
        value = 0
        adr = self._get_address(mode)
        value = self._memory.get_int(adr) + 1
        self._memory.set_hex(
            adr,
            value
        )
        #self._system_dump('INC', mode, args, "Invalid mode number")

        if value == 0:
            self._p = self._p & ( 1 << 1)

        self._pc += args

    def _INX(self, mode, args, cycles):
        '''
        Increment the X register
        X:=X+1
        mode 0
        '''
        self._x = self._x + 1
        #self._system_dump('INX', mode, args, "Invalid mode number")

        if self._x == 0:
            self._p = self._p & ( 1 << 1)
        self._pc += args

    def _INY(self, mode, args, cycles):
        '''
        Increment the Y register
        Y:=Y+1
        modes 0
        '''
        self._y = self._y + 1
        #self._system_dump('INY', mode, args, "Invalid mode number")

        if self._y == 0:
            self._p = self._p & ( 1 << 1)
        self._pc += args

    def _JMP(self, mode, args, cycles):
        '''
        Jump to another location
        PC:={adr}
        modes 8,11
        '''
        self._pc = self._get_address(mode)
        #self._system_dump('JMP', mode, args, "Invalid mode number")
        self._pc += args

    def _JSR(self, mode, args, cycles): # no working
        '''
        Jump to a subroutine
        (S)-:=PC PC:={adr}
        modes 8
        '''
        self._pc += args
        self._memory.set_hex(self._sp, self._pc)
        self._sp -= 1
        self._pc = self._get_address(mode)

        #self._system_dump('JSR', mode, args, "Invalid mode number")
    
    def _KIL(self, mode, args, cycles):
        '''
        Kill program halt CPU
        '''
        self.running = False
        self.interupted = True
        #self._system_dump('KIL', mode, args, "Invalid mode number")

    def _LDA(self, mode, args, cycles):
        '''
        Load accumulator
        A:={adr}
        modes 2,3,4,6,7,8,9,10
        '''
        adr = self._get_address(mode)
        #self._system_dump('LDA', mode, args, "Invalid mode number")
        
        if self._a == 0:
            self._p = self._p & ( 1 << 1)
        self._pc += args

    def _LDX(self, mode, args, cycles):
        '''
        Load X register
        X:={adr}
        modes 2,3,5,8,10
        '''
        adr = self._get_address(mode)
        self._x = self._memory.get_int(adr)
        #self._system_dump('LDX', mode, args, "Invalid mode number")

        if self._x == 0:
            self._p = self._p & ( 1 << 1)
        self._pc += args

    def _LDY(self, mode, args, cycles):
        '''
        Load Y register
        Y:={adr}
        modes 2,3,4,8,9
        '''
        adr = self._get_address(mode)
        self._y = self._memory.get_int(adr)
        #self._system_dump('LDY', mode, args, "Invalid mode number")
        if self._y == 0:
            self._p = self._p & ( 1 << 1)
        self._pc += args


    def _LSR(self, mode, args, cycles):
        '''
        Logical Shift right
        {adr}:={adr}/2
        modes 1,3,4,8,9
        '''
        adr = self._get_address(mode)
        if adr == 0:
            self._p = self._p & ( 1 << 1)
        self._pc += args

    def _NOP(self, mode, args, cycles):
        '''
        No operation
        '''
        #self._system_dump('NOP', mode, args, "Invalid mode number")
        self._pc += args

    def _ORA(self, mode, args, cycles):
        '''
        Logical Inclusive OR
        A:=A or {adr}
        modes 2,3,4,6,7,8,9,10
        '''
        adr = self._get_address(mode)
        self._a = self._a | self._memory.get_int(adr)
        #self._system_dump('ORA', mode, args, "Invalid mode number")

        if self._a == 0:
            self._p = self._p & ( 1 << 1)
        self._pc += args

    def _PHA(self, mode, args, cycles):
        '''
        Pushes a copy of the accumulator on to the stack.
        (S)-:=A
        modes 0
        '''
        self._sp -= 1
        self._memory.set(self._sp, self._a)
        #self._system_dump('PHA', mode, args, "Invalid mode number")

        if self._a == 0:
            self._p = self._p & ( 1 << 1)
        self._pc += args

    def _PHP(self, mode, args, cycles):
        '''
        Push processor status on stack
        (S)-:=P
        modes 0
        '''
        self._sp -= 1
        self._memory.set(self._sp, self._p)
        #self._system_dump('PHP', mode, args, "Invalid mode number")
        self._pc += args

    def _PLA(self, mode, args, cycles):
        '''
        Pull accumulator from stack
        A:=+(S)
        modes 0
        '''
        self._a = self._memory.get_int(self._sp)
        self._sp += 1
        #self._system_dump('PLA', mode, args, "Invalid mode number")
        self._pc += args

    def _PLP(self, mode, args, cycles):
        '''
        Pull processor status from stack
        P:=+(S)
        modes 0
        '''
        self._p = self._memory.get_int(self._sp)
        self._sp += 1
        #self._system_dump('PLP', mode, args, "Invalid mode number")
        self._pc += args

    def _ROL(self, mode, args, cycles):
        '''
        Rotate left
        {adr}:={adr}*2+C
        modes 1,3,4,8,9
        '''
        adr = self._get_address(mode)
        #self._system_dump('ROL', mode, args, "Invalid mode number")

        if self._a == 0:
            self._p = self._p & ( 1 << 1)
        self._pc += args

    def _ROR(self, mode, args, cycles):
        '''
        Rotate right
        {adr}:={adr}/2+C*128
        modes 1.3.4.8.9
        '''
        adr = self._get_address(mode)
        #self._system_dump('ROR', mode, args, "Invalid mode number")

        if self._a == 0:
            self._p = self._p & ( 1 << 1)
        self._pc += args

    def _RTI(self, mode, args, cycles):
        '''
        Return from interrupt
        P,PC:=+(S)
        '''
        if mode == 0:
            pass
            self.interupted = False
        else:
            self._system_dump('RTI', mode, args, "Invalid mode number")
        self._pc += args

    def _RTS(self, mode, args, cycles):
        '''
        Return from subroutine
        PC:=+(S)
        modes 0
        '''
        self._pc = self._memory.get_int(self._sp)
        self._sp += 1
        #self._system_dump('RTS', mode, args, "Invalid mode number")

        self._pc += args

    def _SBC(self, mode, args, cycles):
        '''
        Subtract with Carry
        A:=A-{adr}
        modes 0,3,4,6,7,8,9,10
        '''
        adr = self._get_address(mode)
        #self._system_dump('SBC', mode, args, "Invalid mode number")

        self._p = self._p & ( 1 << 0)
        self._pc += args

    def _SEC(self, mode, args, cycles):
        ''' 
        Set carry flag
        C:=1
        modes 0 
        '''
        self._p = self._p & ( 1 << 0 )
        #self._system_dump('SEC', mode, args, "Invalid mode number")
        self._pc += args

    def _SED(self, mode, args, cycles):
        '''
        Set decimal mode flag
        D:=1
        modes 0
        '''
        self._p = self._p & ( 1 << 3 )
        #self._system_dump('SED', mode, args, "Invalid mode number")
        self._pc += args

    def _SEI(self, mode, args, cycles):
        '''
        Set interrupt disable flag
        I:=1
        modes 0
        '''
        self._p = self._p & ( 1 << 2 )
        #self._system_dump('SEI', mode, args, "Invalid mode number")
        self._pc += args

    def _STA(self, mode, args, cycles):
        '''
        Store Accumulator
        {adr}:=A
        modes 3,4,6,7,8,9,10
        '''
        adr = self._get_address(mode)
        self._memory.set_hex(adr, self._a)
        #self._system_dump('STA', mode, args, "Invalid mode number")
        self._pc += args

    def _STX(self, mode, args, cycles):
        '''
        Store X Register
        {adr}:=X
        modes 3, 5, 8
        '''
        adr = self._get_address(mode)
        self._memory.set_hex(adr, self._x)
        #self._system_dump('STX', mode, args, "Invalid mode number")
        self._pc += args

    def _STY(self, mode, args, cycles):
        '''
        Store Y Register
        {adr}:=Y
        modes 3,4,8
        '''
        adr = self._get_address(mode)
        self._memory.set_hex(adr, self._y)
        #self._system_dump('STY', mode, args, "Invalid mode number")
        self._pc += args

    def _TAX(self, mode, args, cycles):
        '''
        Transfer A to X
        X:=A
        modes 0
        '''
        self._x = self._a
        #self._system_dump('TAX', mode, args, "Invalid mode number")

        if self._x == 0:
            self._p = self._p & ( 1 << 1)
        self._pc += args

    def _TAY(self, mode, args, cycles):
        '''
        Transfer A to Y
        Y:=A
        modes 0
        '''
        self._y = self._a
        #self._system_dump('TAY', mode, args, "Invalid mode number")

        if self._y == 0:
            self._p = self._p & ( 1 << 1)
        self._pc += args

    def _TSX(self, mode, args, cycles):
        '''
        Transfer S to X
        X:=S
        modes 0
        '''
        self._x = self.s
        #self._system_dump('TSX', mode, args, "Invalid mode number")

        if self._x == 0:
            self._p = self._p & ( 1 << 1)
        self._pc += args

    def _TXA(self, mode, args, cycles):
        '''
        Transfer X to A
        A:=X
        modes 0
        '''
        self._a = self._x
        #self._system_dump('TXA', mode, args, "Invalid mode number")

        if self._a == 0:
            self._p = self._p & ( 1 << 1)
        self._pc += args

    def _TXS(self, mode, args, cycles):
        '''
        Transfer X to S
        S:=X
        modes 0
        '''
        self.s = self._x
        #self._system_dump('TXS', mode, args, "Invalid mode number")
        self._pc += args
        
    def _TYA(self, mode, args, cycles):
        '''
        Transfer Y to A
        A:=Y
        modes 0
        '''
        self._a = self._y
        #self._system_dump('TYA', mode, args, "Invalid mode number")

        if self._a == 0:
            self._p = self._p & ( 1 << 1)
        self._pc += args