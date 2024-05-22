import struct
import sys
if len(sys.argv) != 2 or sys.argv[1] == "--help":
    exit("usage: python pyriscv.py <binary-file>")

filename = sys.argv[1]
with open(filename, mode="rb") as file:
    mem = bytearray(file.read())

regs = [0]*32
pc = [0]

def step(pc, regs, mem):
    def btoi(binary, signed=False):  # binary to int
        if signed and binary[-1] == "1": return btoi(binary.ljust(32, "1"))
        return int(binary[::-1], base=2)
    def wrap(num):
        return num % 2**32
    def signed(num):
        return wrap(num+2**31) - 2**31

    regs[0] = 0
    assert pc[0] % 4 == 0, ["unaligned pc", pc]
    for i, value in enumerate(regs):
        assert value in range(0, 2**32), ["invalid register value", regs, i]
    instr = format(struct.unpack("<I", mem[pc[0]:pc[0]+4])[0], "032b")[::-1]
    opcode = instr[0:7]

    if opcode == "1100110":  # R-type math operations
        rd = btoi(instr[7:12])
        funct3 = btoi(instr[12:15])
        rs1 = btoi(instr[15:20])
        rs2 = btoi(instr[20:25])
        funct7 = btoi(instr[25:32])
        if [funct3, funct7] == [0x0, 0x00]:  # ADD
            regs[rd] = wrap(regs[rs1] + regs[rs2])
        elif [funct3, funct7] == [0x0, 0x20]:  # SUB
            regs[rd] = wrap(regs[rs1] - regs[rs2])
        elif [funct3, funct7] == [0x1, 0x00]:  # SLL
            regs[rd] = wrap(regs[rs1] << (regs[rs2]%32))
        elif [funct3, funct7] == [0x2, 0x00]:  # SLT
            regs[rd] = 1 if (regs[rs1]^(1<<31) < regs[rs2]^(1<<31)) else 0
        elif [funct3, funct7] == [0x3, 0x00]:  # SLTU
            regs[rd] = 1 if (regs[rs1] < regs[rs2]) else 0
        elif [funct3, funct7] == [0x4, 0x00]:  # XOR
            regs[rd] = regs[rs1] ^ regs[rs2]
        elif [funct3, funct7] == [0x5, 0x00]:  # SRL
            regs[rd] = regs[rs1] >> (regs[rs2]%32)
        elif [funct3, funct7] == [0x5, 0x20]:  # SRA
            regs[rd] = wrap(signed(regs[rs1]) >> (regs[rs2]%32))
        elif [funct3, funct7] == [0x6, 0x00]:  # OR
            regs[rd] = regs[rs1] | regs[rs2]
        elif [funct3, funct7] == [0x7, 0x00]:  # AND
            regs[rd] = regs[rs1] & regs[rs2]
        else:
            assert False, [opcode, funct3, funct7]

    elif opcode == "1100100":  # I-type math operations
        rd = btoi(instr[7:12])
        funct3 = btoi(instr[12:15])
        rs1 = btoi(instr[15:20])
        funct7 = btoi(instr[25:32])
        imm = wrap(btoi(instr[20:32], signed=True))
        if [funct3] == [0x0]:  # ADDI
            regs[rd] = wrap(regs[rs1] + imm)
        elif [funct3, funct7] == [0x1, 0x00]:  # SLLI
            regs[rd] = wrap(regs[rs1] << (imm%32))
        elif [funct3] == [0x2]:  # SLTI
            regs[rd] = 1 if (regs[rs1]^(1<<31) < imm^(1<<31)) else 0
        elif [funct3] == [0x3]:  # SLTIU
            regs[rd] = 1 if (regs[rs1] < imm) else 0
        elif [funct3] == [0x4]:  # XOR
            regs[rd] = regs[rs1] ^ imm
        elif [funct3, funct7] == [0x5, 0x00]:  # SRL
            regs[rd] = regs[rs1] >> (imm%32)
        elif [funct3, funct7] == [0x5, 0x20]:  # SRA
            regs[rd] = wrap(signed(regs[rs1]) >> (imm%32))
        elif [funct3] == [0x6]:  # OR
            regs[rd] = regs[rs1] | imm
        elif [funct3] == [0x7]:  # AND
            regs[rd] = regs[rs1] & imm
        else:
            assert False, [opcode, funct3, funct7]

    elif opcode == "1100000":  # I-type load operations
        rd = btoi(instr[7:12])
        funct3 = btoi(instr[12:15])
        rs1 = btoi(instr[15:20])
        imm = btoi(instr[20:32], signed=True)
        i = wrap(regs[rs1] + imm)
        if [funct3] == [0x0]:  # LB
            regs[rd] = wrap(struct.unpack("<b", mem[i:i+1])[0])
        elif [funct3] == [0x1]:  # LH
            regs[rd] = wrap(struct.unpack("<h", mem[i:i+2])[0])
        elif [funct3] == [0x2]:  # LW
            regs[rd] = wrap(struct.unpack("<i", mem[i:i+4])[0])
        elif [funct3] == [0x4]:  # LBU
            regs[rd] = struct.unpack("<B", mem[i:i+1])[0]
        elif [funct3] == [0x5]:  # LHU
            regs[rd] = struct.unpack("<H", mem[i:i+2])[0]
        else:
            assert False, [opcode, funct3]

    elif opcode == "1100010":  # S-type store operations
        rd = btoi(instr[7:12])
        funct3 = btoi(instr[12:15])
        rs1 = btoi(instr[15:20])
        rs2 = btoi(instr[20:25])
        imm = btoi(instr[7:12] + instr[25:32], signed=True)
        i = wrap(regs[rs1] + imm)
        if [funct3] == [0x0]:  # SB
            mem[i:i+1] = struct.pack("<I", regs[rs2])[:1]
        elif [funct3] == [0x1]:  # SH
            mem[i:i+2] = struct.pack("<I", regs[rs2])[:2]
        elif [funct3] == [0x2]:  # SW
            mem[i:i+4] = struct.pack("<I", regs[rs2])[:4]
        else:
            assert False, [opcode, funct3]

    elif opcode == "1100011":  # B-type branch operations
        funct3 = btoi(instr[12:15])
        rs1 = btoi(instr[15:20])
        rs2 = btoi(instr[20:25])
        imm = btoi("0" + instr[8:12] + instr[25:31] + instr[7] + instr[31], signed=True)
        if [funct3] == [0x0]:  # BEQ
            if regs[rs1] == regs[rs2]: pc[0] = wrap(pc[0] + imm); return
        elif [funct3] == [0x1]:  # BNE
            if regs[rs1] != regs[rs2]: pc[0] = wrap(pc[0] + imm); return
        elif [funct3] == [0x4]:  # BLT
            if regs[rs1]^(1<<31) < regs[rs2]^(1<<31): pc[0] = wrap(pc[0] + imm); return
        elif [funct3] == [0x5]:  # BGE
            if regs[rs1]^(1<<31) >= regs[rs2]^(1<<31): pc[0] = wrap(pc[0] + imm); return
        elif [funct3] == [0x6]:  # BLTU
            if regs[rs1] < regs[rs2]: pc[0] = wrap(pc[0] + imm); return
        elif [funct3] == [0x7]:  # BGEU
            if regs[rs1] >= regs[rs2]: pc[0] = wrap(pc[0] + imm); return
        else:
            assert False, [opcode, funct3]

    elif opcode == "1111011":  # J-type JAL
        rd = btoi(instr[7:12])
        imm = btoi("0" + instr[21:31] + instr[20] + instr[12:20] + instr[31], signed=True)
        regs[rd] = wrap(pc[0] + 4); pc[0] = wrap(pc[0] + imm); return

    elif opcode == "1110011":  # I-type
        rd = btoi(instr[7:12])
        funct3 = btoi(instr[12:15])
        rs1 = btoi(instr[15:20])
        imm = btoi(instr[20:32], signed=True)
        if funct3 == 0:  # JALR
            regs[rd] = wrap(pc[0] + 4); pc[0] = wrap(regs[rs1] + imm); return
        else:
            assert False, [opcode, funct3]

    elif opcode == "1110110":  # U-type LUI
        rd = btoi(instr[7:12])
        imm = btoi(instr[12:31])
        regs[rd] = imm << 12

    elif opcode == "1110100":  # U-type AUIPC
        rd = btoi(instr[7:12])
        imm = btoi(instr[12:31])
        regs[rd] = wrap(pc[0] + (imm << 12))

    elif opcode == "1111000":  # I-type FENCE
        rd = btoi(instr[7:12])
        funct3 = btoi(instr[12:15])
        rs1 = btoi(instr[15:20])
        imm = btoi(instr[20:32])
        return "fence", [rd, rs1, imm]

    elif opcode == "1100111":  # I-type
        rd = btoi(instr[7:12])
        funct3 = btoi(instr[12:15])
        rs1 = btoi(instr[15:20])
        imm = btoi(instr[20:32], signed=True)
        if [rd, rs1, funct3] == [0, 0, 0x0]:  # SYSTEM
            return "system", imm
        else:
            assert False, [opcode, rd, rs1, funct3]

    else:
        assert False, [opcode]

    pc[0] = wrap(pc[0] + 4)

# ECALL: If a7 is 0, one byte of input is read into a0. If a7 is 1, a0 is
# output. If a7 is 2, the emulator exits. FENCEs are ignored, and EBREAKs call
# breakpoint().
while True:
    out = step(pc, regs, mem)
    if out is None:
        continue
    elif out[0] == "system":
        if out[1] == 0:
            syscall = regs[17]  # reg a7
            if syscall == 0:
                char = sys.stdin.buffer.read(1)
                if char:
                    regs[10] = char[0]  # reg a0 = input
            elif syscall == 1:
                sys.stdout.buffer.write(bytes([regs[10]]))  # output reg a0
                sys.stdout.buffer.flush()
            elif syscall == 2:  # halt
                break
            else:
                assert False, ["syscall", syscall]
            pc[0] = (pc[0] + 4) % 2**32
        elif out[1] == 1:
            breakpoint()
            pc[0] = (pc[0] + 4) % 2**32
        else:
            assert False, ["system", out[1]]
    elif out[0] == "fence":
        # no threading yet, so this is a no-op
        pc[0] = (pc[0] + 4) % 2**32
    else:
        assert False, ["return", out[0]]
