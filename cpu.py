"""CPU functionality."""
# LDI: load "immediate", store a value in a register, or "set this register to this value".
# PRN: a pseudo-instruction that prints the numeric value stored in a register.
# HLT: halt the CPU and exit the emulator.
import sys

# setup consts for op codes
LDI = 0b10000010  # LDI R0,8 130 set value of a register to an integer
PRN = 0b01000111  # register, print numeric value PRN R0, 71
HLT = 0b00000001  # HLT
MUL = 0b10100010  # Multiply
ADD = 0b10100000  # Addition
PUSH = 0b01000101  # push register
POP = 0b01000110  # pop value at rhe top of the stack into given register
RET = 0b00010001  # return from subroutine
CALL = 0b01010000

XOR = 0b10101011  # bitwise-xor
AND = 0b10101000  # Bitwise-AND the values in registerA and registerB, then store the result in registerA.
CMP = 0b10100111  # Compare the values in two registers.
JMP = 0b01010100  # jump to the address stored in the given register
# If E flag is clear (false, 0), jump to the address stored in the given register.
JNE = 0b01010110
# if equal flag is set (true), jump to the address stored in the given register.
JEQ = 0b01010101
PRA = 0b01001000  # register print alpha character value stored in register

# SP = 7  # stack pointer set to be used a R7 per spec


class CPU:
    def __init__(self):
        """Construct a new CPU."""
        # create 256 bites of memory
        self.ram = [0] * 256
        # 8 bit registers
        self.registers = [0] * 8
        # program counter PC
        self.pc = 0
        # set Stack Pointer 'PC' index in register, it will always point to position  in Register
        self.sp = 7
        # assign SP to the value of 244 in RAM
        self.registers[7] = 0xF4
        # dictionary to hold Flags
        self.flags = {}
        # Build branch table:
        self.bt = {}
        self.bt[JMP] = self.jmp
        self.bt[JNE] = self.jne
        self.bt[ADD] = self.add
        self.bt[MUL] = self.mul
        self.bt[PUSH] = self.push
        self.bt[POP] = self.pop
        self.bt[CALL] = self.call
        self.bt[JEQ] = self.jeq
        self.bt[CMP] = self.cmp_func
        self.bt[AND] = self.and_func
        self.bt[XOR] = self.xor_func
        self.bt[LDI] = self.ldi
        self.bt[PRN] = self.prn
        self.bt[PRA] = self.pra
        self.bt[RET] = self.ret


    def load(self, file_name):
        """Load a program into memory."""
        address = 0
        program = []

        try:
            address = 0
            # open the file
            with open(file_name) as f:
                for line in f:
                    # strip out the white space at a inline comment
                    clean_line = line.split('#')
                    # grab string number
                    num = clean_line[0].strip()

                    # check if val is blank or not, if it is skip to next line
                    if num == '':
                        continue
                    # number string to integer
                    value = int(num, 2) # we need to convert a binary string to a number ex. "100000010"
                    program.append(value)

        except FileNotFoundError:
            print(f"{sys.argv[0]}: {file_name} ERR: FILE NOT FOUND")
            sys.exit(1)

        for instruction in program:
            self.ram[address] = instruction
            address += 1

    # ALU to perform arithmatic operations and CMP operations
    def alu(self, op, reg_a, reg_b): # arithmetic logic unit ALU
        """ALU operations."""
        # variables used for flagging
        a = self.registers[reg_a]
        b = self.registers[reg_b]

        if op == "ADD": # addition
            self.registers[reg_a] += self.registers[reg_b]
        # elif op == "SUB": # subtraction 
        #     self.registers[reg_a] -= self.registers[reg_b]
        elif op == 'MUL': # multiplication
            self.registers[reg_a] *= self.registers[reg_b]
        # elif op == 'DIV': # division 
        #     self.registers[reg_a] /= self.registers[reg_b]
        elif op == 'AND':
            self.registers[reg_a] = self.registers[reg_a] and self.registers[reg_b]
        elif op == 'XOR':  # bitwise exclusive or
            self.registers[reg_a] = self.registers[reg_a] ^ self.registers[reg_b]
        elif op == 'CMP':
            if a == b:
                self.flags['E'] = 1
            else:
                self.flags['E'] = 0
            if a < b:
                self.flags['L'] = 1
            else:
                self.flags['L'] = 0
            if a > b:
                self.flags['G'] = 1
            else:
                self.flags['G'] = 0
        else:
            raise Exception("Unsupported ALU operation")

        # Inside the CPU, there are two internal registers used for memory operations:
    # the Memory Address Register (MAR) and the Memory Data Register (MDR).
    def ram_read(self, mar):
        return self.ram[mar]

    def ram_write(self, mar, mdr):
        self.ram[mar] = mdr

    def jne(self, a=None, b=None):
        if self.flags['E'] == 0:
            self.pc = self.registers[a]
        else:
            self.pc += 2

    def jeq(self, a=None, b=None):
        if self.flags['E'] == 1:
            self.pc = self.registers[a]
        else:
            self.pc += 2

    def jmp(self, a=None, b=None):
        self.pc = self.registers[a]

    # Compare the values in two registers.
    def cmp_func(self, a=None, b=None):
        self.alu("CMP", a, b)

    def and_func(self, a=None, b=None):
        self.alu("AND", a, b)

    def xor_func(self, a=None, b=None):
        self.alu("XOR", a, b)

    def mul(self, a=None, b=None):
        self.alu("MUL", a, b)

    def push(self, a=None, b=None):
        self.registers[self.sp] -= 1
        val = self.registers[a]
        self.write_ram(self.registers[self.sp], val)

    def pop(self, a=None):
        val = self.read_ram(self.registers[self.sp])
        self.registers[a] = val
        self.registers[self.sp] += 1

    def call(self, b=None):
        val = self.pc + 2
        self.registers[self.sp] -= 1
        self.write_ram(self.registers[self.sp], val)
        reg = self.read_ram(self.pc + 1)
        addr = self.registers[reg]
        self.pc = addr

    # Set the value of a register to an integer.
    def ldi(self, a=None, b=None):
        self.registers[a] = b

    # Print numeric value stored in the given register.
    def prn(self, a=None, b=None):
        print(self.registers[a])

    def pra(self, a=None, b=None):
        print(chr(self.registers[a]))

    def add(self, a=None, b=None):
        self.alu("ADD", a, b)

    # Return from subroutine.
    def ret(self):
        ret_addr = self.registers[self.sp]
        self.pc = self.read_ram(ret_addr)
        self.registers[self.sp] += 1

    def run(self):
        """Run the CPU."""
        # hold instructions
        jump = [CALL, JNE, JEQ, JMP, RET]

        # run the program
        while True:
            # Instruction Register, read from ram
            IR = self.ram_read(self.pc)
            # assign the operands
            operand_a = self.ram_read(self.pc + 1)
            operand_b = self.ram_read(self.pc + 2)

            if IR == HLT:
                print('Program exiting! \n')
                sys.exit(1)
            elif IR in jump:
                self.bt[IR](operand_a, operand_b)
            elif IR in self.bt:
                self.bt[IR](operand_a, operand_b)
                self.pc += (IR >> 6) + 1
            else:
                print(IR)