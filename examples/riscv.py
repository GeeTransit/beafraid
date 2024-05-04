"""A simple RISC-V 32I emulator in Brainfuck

Run `python -m examples.riscv --output-mainloop` to output just the mainloop,
where SYSTEM and FENCE instructions break out of the mainloop. If all memory is
zero, PC should start at offset 192 (since the movable head is 64 bytes).

`--output-simple-iobreak` outputs the mainloop along with wrapper code that
handles ECALLs, where a7 is either 0, 1, or 2. If a7 is 0, one byte of input is
read into a0. If a7 is 1, a0 is output. If a7 is 2, the wrapper code exits.
FENCEs are ignored, and EBREAKs trigger a memory dump if the Brainfuck
interpreter supports `#`.

`--convert-hex-to-bf` takes in big-endian hex-encoded data with 4 bytes on each
line, and outputs Brainfuck code that initializes memory to the data.
`--offset` specifies where to start initializing memory, which is 256 if using
the `--output-simple-iobreak` wrapper code (64 from wrapper, 64 from the head,
128 from registers).

```shell
python -m examples.riscv --convert-hex-to-bf --offset 256 < in.hex > out.bf
python -m examples.riscv --output-simple-iobreak >> out.bf
```

An example in.hex which echoes out all input given, with line buffering:
```
# .text
#         li  t2, 10
# main:   la  t0, buff
#         li  a7, 0
# loopR:  li  a0, 0
#         ecall
#         beq  a0, zero, endM
#         sb  a0, (t0)
#         addi  t0, t0, 1
#         bne  a0, t2, loopR
#         jal  write
#         j  main
# endM:   jal  write
#         li  a7, 2
#         ecall
# write:  la  t1, buff
# loopW:  beq  t1, t0, endW
#         lb  a0, (t1)
#         li  a7, 1
#         ecall
#         addi  t1, t1, 1
#         j  loopW
# endW:   ret
# .data
# buff:
# .zero 100
00a00393
00000297
05c28293
00000893
00000513
00000073
00050c63
00a28023
00128293
fe7516e3
014000ef
fd9ff06f
00c000ef
00200893
00000073
00000317
02430313
00530c63
00030503
00100893
00000073
00130313
fedff06f
00008067
```

"""
from beafraid import *
from .s4op import pack_class_offsets, shift, size, start, Struct, Offset


import sys
_DEBUG_ = False
if __name__ == "__main__":
    if "--debug" in sys.argv:
        _DEBUG_ = True



def join(*parts):
    if len(parts) == 1:
        if type(parts[0]) is str:
            parts = [parts[0]]
        else:
            parts = parts[0]
    return "\n".join(parts)

r'''
[ general design ]

Layout: head[64] registers[4*32] memory[...]

Detailed layout:
                                              46   51      59
                                         _____ijkl___fnIJKLxyzw_
                                        40  44
                        24              M   S12dimm.
                        rs1.rs2.rd..pc..__FN
___-aA1_bB1_cC1_dD1_____
xy-_aAbBcCdDeEfFgGhH__
                        abcdABCD

riscv is little endian
no memory wraparound unfortunately (accessing higher memory is incredibly slow)
ebreak and ecall exit with E_SYSTEM
    differentiate based on rs2 (0=ecall, 1=ebreak)
    in --output-simple-iobreak, ecall handles i/o and breaking out of mainloop
fences exit with E_FENCE
    all memory reads/writes are already immediately visible
    there are also no threads since we are "single core"
    however, one could repurpose this to flush i/o maybe
TODO: generate exception on misaligned jumps (check lower two bits after jumps/branches)

bitwise operations are done by operating on interleaved bits and packing after
bitwise shifts are done by first shifting 8 bits at a time (aka shifting bytes)
    and then doing the individual bits (need to handle carry and such)
signed comparisons simply mean flip the msb (in brainfuck, we can add 128)
sign extension are done in two places
    for immediates, we check the msb of the instruction
    for loads, we compare the msbyte with 128

memory accesses are done by shifting a block of memory right/left to the desired location
    reading or writing using the space just after the block
    and shifting back to the original location
    note that this means the location of the pointer is temporarily restricted to the block
    and the switch case now acts inside the block

the mainloop is a giant switch case
    0 is fetch (to make starting the interpreter simpler)
    at the end of each case, the pointer moves left 2 spaces
        these 2 spaces are assumed to be always zero
    at the end of the whole switch, the pointer moves right 2 spaces
    we guarantee 1 is increment pc (to make SYSTEM calls easier)
    we also guarantee -1 (or 255) is the break case

TODO: potentially shrink head size
aim for 32 cells
especially since the memory head is unused when doing other operations

TODO: move landing into _t like below to shorten
__fn_ijkl_IJKLxyzw_
n is moved temporarily during read/writes?

TODO: shift large blocks of memory by shifting left 2 and then shifting right 3
similarly, shift left 3 and then shift right by 2 also works
__abcdef_
_6abcdef_
a_5bcdef_
ab_4cdef_
...
abcdef_0_
abcdef_6_
abcde_5_f
abcd_4_ef
...
_0_abcdef
___abcdef
'''


# Special constants for N when the mainloop exits
E_SYSTEM = 0x07
E_FENCE = 0x08
# The constants below are subject to change (might be combined in the future)
E_INVALID_OPCODE_1 = 0x09
E_INVALID_OPCODE_2 = 0x0A
E_INVALID_OPCODE_3 = 0x0B
E_INVALID_OPCODE_4 = 0x0C
E_INVALID_OPCODE_5 = 0x0D
E_DECODE_MISSING_ERRNO = 0x0E  # Should never happen
E_INVALID_FUNCT3_LOAD = 0x14
E_INVALID_FUNCT3_STORE = 0x15
E_INVALID_FUNCT7_MATH_R = 0x16
E_INVALID_FUNCT7_MATH_I = 0x17
E_INVALID_FUNCT3_MATH = 0x18  # Used twice
E_INVALID_FUNCT3_BRANCH = 0x19


_next_function_index = 0
functions = []
def mark_function(func):
    global _next_function_index
    if _next_function_index == 255:
        # -1 is taken up by ERROR.f
        raise RuntimeError("cannot have more than 254 functions")
    if isinstance(func, str):
        class _forward_function:
            def __init__(self, f, name):
                self.f = f
                self.name = name
            def __call__(self):
                assert False, "cannot expand a forward declared function"
        func = _forward_function(f=_next_function_index, name=func)
        functions.append(func)
    else:
        replaced = False
        for i, function in enumerate(functions):
            if getattr(function, "name", None) == func.__name__:
                functions[i] = func
                func.f = function.f
                replaced = True
                break
        else:
            func.f = _next_function_index
        if func.f == 0:
            assert func.__name__ == "FETCH"
        if replaced:
            return func
        functions.append(func)
    _next_function_index += 1
    return func

@mark_function  # case FETCH:  # FETCH == 0
def FETCH():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(  # fetch
        at(h.F, s(0)),
        at(shift(range(4), size(h)), s(0)),  # clear x0 register
        zipmove(h.pc, [*h.land, h.F, h.N]),  # pc.. => __FN
        zipmove([*h.land, h.F, h.N], zip(h.pc, md.i)),  # __FN => pc.. + IJKL
        # j -= 2; { while j: j -= 2; i += 1 }; i += 1  # i += 128
        at(h.mem.i[1], s(-2)),
        loop(h.mem.i[1], at(h.mem.i[1], c(-2)) + at(h.mem.i[0], c(1))),
        at(h.mem.i[0], c(1)),
        at(md.f, s(MEMMOVRIGHT.f)),  # f = MEMMOVRIGHT
        at(h.mem_next, s(DECODE.f)),  # M = DECODE
        g(-h.F + md.f),  # land __fn
    )

class ERROR: f = -1  # mainloop stop condition
INCPC = mark_function("INCPC")  # INCPC == 1

assert ERROR.f == -1
assert FETCH.f == 0
assert INCPC.f == 1

@mark_function
def MEMMOVRIGHT():  # case MEMMOVRIGHT:
    # Note: This function is called at __fn
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        g(-md.f + h.F), # reset to __FN
        MemoryMove.code(head=h.mem),
        at(md.f, s(0)), move(md.n, md.f),  # f = 0; n => f
        switch(on=md.f)(  # switch f:
            join(  # default:  # 1 or 2
                ReadWrite.code(rwp=md.rwp),
                at(md.f, c(-1)), move(md.f, md.n),  # f -= 1; f => n
                at(md.f, s(MEMMOVLEFT.f)),  # f = MEMMOVLEFT
                zipmove(md.i, h.mem.i),  # IJKL => ijkl
                g(-2),  # land __fn
            ),
            join(  # case 0:
                zipmove(md.i, [*md.land, md.f, md.n]),  # IJKL => __fn
                zipmove([*md.land, md.f, md.n], zip(md.i, h.mem.i)),  # __fn => IJKL + ijkl
                at(md.f, s(MEMMOVRIGHT.f)),  # f = MEMMOVRIGHT
                at(md.n, s(2)),  # n = 2
                g(-2),  # land __fn
            ),
        ),
        g(+2),
        g(-h.F + md.f)  # reset to __fn
    )

@mark_function
def MEMMOVLEFT():  # case MEMMOVLEFT:
    # Note: This function is called at __fn
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        g(-md.f + h.F), # reset to __FN
        MemoryMove.code(head=h.mem, rightward=False),
        at(md.f, s(0)), move(md.n, md.f),  # f = 0; n => f
        switch(on=md.f)(  # switch f:
            join(  # default:
                # j -= 2; { while j: j -= 2; i += 1 }; i += 1  # i += 128
                at(h.mem.i[1], s(-2)),
                loop(h.mem.i[1], at(h.mem.i[1], c(-2)) + at(h.mem.i[0], c(1))),
                at(h.mem.i[0], c(1)),
                at(md.f, s(MEMMOVLEFT.f)),  # f = MEMMOVLEFT
                at(md.n, s(0)),  # n = 0
                g(-2),  # land __fn
            ),
            join(  # case 0:
                at(h.F, s(0)), move(h.mem_next, h.F),  # M => F
                g(-md.f + h.F - 2),  # land __FN
            ),
        ),
        g(+2),
        g(-h.F + md.f)  # reset to __fn
    )

@mark_function
def ADD():  # case ADD:
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        zipmove(h.rs1, h.ints.num1[:4]),  # rs1. => abcd
        zipmove(h.rs2, h.ints.num2[:4]),  # rs2. => ABCD
        ifnonzero(h.funct_flag,  # if S:
            # S = 0
            h.ints.negate2()
        ),
        h.ints.add1to2(),
        zipmove(h.ints.num2[:4], h.rs2),  # ABCD => rs2.
        at(h.ints.num2[-1], s(0)),  # clear ____[1]
        at(h.F, s(0)), move(h.N, h.F),  # N => F
    )

@mark_function
def LESSTHAN():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        zipmove(h.rs1, h.ints.num1[:4]),  # rs1. => abcd
        zipmove(h.rs2, h.ints.num2[:4]),  # rs2. => ABCD
        h.ints.compare1lessthan2(),
        move(h.ints.num2[0], h.rs2[0]),  # ABCD => rs2.
        at(h.F, s(0)), move(h.N, h.F),  # N => F
    )

def ON_MAINLOOP():
    BB, F, N, xyzw, S, x1, x2, xd, imm = DECODE_ARGS()
    return at(F, "#")
def MAINLOOP():
    BB, F, N, xyzw, S, x1, x2, xd, imm = DECODE_ARGS()
    return loopuntil(ERROR.f, F, join(  # while F != -1:
        ON_MAINLOOP(),
        switch(on=F)(  # switch F:
            at(F, s(ERROR.f)) + g(-2),
            *[[func.f, func() + g(-2)] for func in functions if not hasattr(func, "name")],
        ),
        g(+2),
    ))

r'''
[0 = fetch function]
set memory flag to decode
memory move right
    set pointer to 128
    set data pointer to pc
    set n = 1
[decode function]
increment pc by 4
clear x0 register
separate step?: terminate if all bytes are 0
decode instruction
    try to lessen code duplication, meaning split before processing
    get opcode, rd/imm[4:0], funct3, rs1, rs2, funct7/imm[11:5]
    switch on opcode
        switch on funct3 and funct7 if necessary
        do shit with the parts (rd/rs1/whatever) with first space as temp if needed
        if load
            calculate rs1 + imm[11:0]
            set memory flag to load
            memory move right
                set pointer to 128
                set data pointer to rs1 + imm[11:0]
                set n = 1
            [load function]
            switch on funct3
                if 0 (load byte)
                    rd[3:1] = 0
                    rd[0] = data[0]
                    copy rd[0]
                    unpack rd[0] to bits
                    if msb, rd[3:2] = -1
                    clear bits
                if 1 (load half)
                    rd[3:2] = 0
                    rd[1:0] = data[1:0]
                    copy rd[1]
                    unpack rd[1] to bits
                    if msb, rd[3:2] = -1
                    clear bits
                if 2 (load word)
                    rd[3:0] = data[3:0]
                if 4 (load byte unsigned)
                    rd[3:1] = 0
                    rd[0] = data[0]
                if 5 (load half unsigned)
                    rd[3:2] = 0
                    rd[1:0] = data[1:0]
            set memory flag to fetch
            memory move right
                set pointer to 4*rd
                set data pointer to 4*rd
                set n = 2
        if store
            switch on funct3
                if 0 (store byte)
                    data[4:2] = 0
                    data[1] = 1
                    data[0] = rs2[0]
                if 1 (store half)
                    data[4:3] = 0
                    data[2] = 1
                    data[1:0] = rs2[1:0]
                if 2 (store word)
                    data[4] = 1
                    data[3:0] = rs2[3:0]
            calculate rs1 + imm[11:0]
            set memory flag to fetch
            memory move right
                set pointer to 128
                set data pointer to rs1 + imm[11:0]
                set n = 1
        if math
            if opcode == 0010011 (ig subtract this and check zero?)
                rs2 = imm[0:11]
            switch on funct3
                if 0 (add/sub)
                    working addition algorithm
                        move rs1 to abcd
                        move rs2 to ABCD
                        - aA1_ bB1_ cC1_ dD1_ ____
                        if funct7
                            bitwise negate rs2
                            https://esolangs.org/wiki/Brainfuck_algorithms#x%C2%B4_=_not_x_(bitwise)
                            -> >>>> >>>> >>>> >>> +[-<< +[->-<]>[-<+>]<< <+]
                        calculate rs1 + rs2
                        -> >>>> >>>> >>>> >>>
                        +[- <<<d
                        [->>+<+[>-]> [[->>>>++<+[>--]>] <+[-<<<<] >] <<<]
                        <+]
                        move ABCD to rd
                        clear msb
                    old "algo" yea fuck it imma just unpack cuz i cant be asked to think about this further
                        for each byte
                            unpack rs1 and rs2
                            if funct7
                                negate rs2
                            aA_bB_...hH___-
                            >>++[-<< [->+<]>[<+>-[>>+<<<->-[<+>[-]]]] >>>>+]<<<-[+<<<-]<<
                            pack rd
                            move last bit to start
                            >>> +>>> >>> >>> >>> >>> >>> >>> [- <<< <<< <<< <<< <<< <<< <<< <+>>] -[+<-] <<<
                if 1 (shift left logical)
                if 2 (set less than signed)
                    set signed flag
                    call lessthan function
                        set n = 1
                if 3 (set less than unsigned)
                    call lessthan function
                        set n = 1
                if 4 (xor)
                    for each byte
                        xy-_aAbB..hH__
                        move rs1[i] to x
                        [->>>>[>>]++[-<<]<<]
                        move rs2[i] to y
                        > [->>>>[>>]++[-<<]<<]<
                        >>->>>>>>>>>>>>>>>>
                        go down each bit, xoring along the way
                        +[- [->+<]> [<+>-[<->[-]]] >[-<<++>>]<<<< +]-
                        +>>[-<<<<+>>>>]<<<<
                        move x to rd[i]
                if 5 (shift right logical/arithmetic)
                    if funct7 arithmetic
                if 6 (or)
                    for each byte
                        xy-_aAbB..hH__
                        move rs1[i] to x
                        [->>>>[>>]++[-<<]<<]
                        move rs2[i] to y
                        > [->>>>[>>]++[-<<]<<]<
                        >>->>>>>>>>>>>>>>>>
                        go down each bit, oring along the way
                        +[- [->+<]> [<+>[-]] >[-<<++>>]<<<< +]-
                        +>>[-<<<<+>>>>]<<<<
                        move x to rd[i]
                if 7 (and)
                    for each byte
                        xy-_aAbB..hH__
                        move rs1[i] to x
                        [->>>>[>>]++[-<<]<<]
                        move rs2[i] to y
                        > [->>>>[>>]++[-<<]<<]<
                        >>->>>>>>>>>>>>>>>>
                        go down each bit, anding along the way
                        +[- [->+<]> [-[<+>[-]]] >[-<<++>>]<<<< +]-
                        +>>[-<<<<+>>>>]<<<<
                        move x to rd[i]
            set memory flag to fetch
            memory move right
                set pointer to 4*rd
                set data pointer to 4*rd
                set n = 2
        if branch
            switch on funct3
                if 0 (==)
                    set comparison type to equal
                if 1 (!=)
                    set comparison type to equal
                    set negate to true
                if 4 (< signed)
                    set comparison type to less than
                    set signed to true
                if 5 (>= signed)
                    set comparison type to less than
                    set signed to true
                    set negate to true
                if 6 (< unsigned)
                    set comparison type to less than
                if 7 (>= unsigned)
                    set comparison type to less than
                    set negate to true
            if signed
                add 128 to msb of both rs1 and rs2
                https://esolangs.org/wiki/Brainfuck_constants#128
                > >>>> >>>> >>>> >>--[--<+<+>>]<+<+ <<<< <<<< <<<< <
            switch on comparison type
                if equal
                    abcdABCD
                    [->>>>-<<<<]> [->>>>-<<<<]> [->>>>-<<<<]> [->>>>-<<<<]>
                    [[-]>[-]+<]> [[-]>[-]+<]> [[-]>[-]+<]>
                    [[-] <<< <<<< - >>>> >>>]
                    <<< <<<< +
                    call aftercompare function
                if less than
                    call lessthan function
                        set n = 2
            [aftercompare function]
            if negate
                rs1[0] = 1 - rs1[0]
                [->+<]+>[-<->]<
            if rs1[0]
                add imm[11:0] to pc
            call fetch function
        if jump
        if lui/auipc
        if ecall/ebreak


unsigned 127 < 128
signed 127 >= -128


make 128 in a cell (with temp beside)
at(1, -- [ -- at(-1, -) ] ) -


[ procedures ]

to read/write a register
    memory move right ( pointer = 4*register ; data pointer = 4*register ; n = 2 )
to read/write an address
    memory move right ( pointer = 128 ; data pointer = address ; n = 1 )

free lite panes

[ functions ]

lessthan function
    n = 1 or 2
    move rs1 to abcd
    move rs2 to ABCD
    - aA1_ bB1_ cC1_ dD1_
    -> >>>> >>>> >>>> >>>
    for each byte of rs1 and rs2
    +[- <<<d
        inspiration from https://www.codingame.com/playgrounds/50455/brainfuck-part-4---advanced-maths/compare-integers
        [->>+< [->-] > [<<[-]>>>] <<<]
        if D, then d < D
        if 1, then d > D
        >[[-]<< +[- <<[-]<[-]< +]- >+>]
        >[-<<< +[- <<[-]<[-]< +]- >>>]
        <<[->+<]
    <+]
    move ABCD to rd
    if n == 1
        set memory flag to fetch
        memory move right
            set pointer to 4*rd
            set data pointer to 4*rd
            set n = 2
    if n == 2
        call aftercompare function

memory move right
    0 0 f n will be first 4 cells of data
    move right according to pointer
    if n == 1
        memory move right ( pointer = data pointer ; n = 3 )
    if n == 2 or 3
        read/write
        memory move left ( pointer = data pointer ; n = n-1 )

memory move left
    0 0 f n will be first 4 cells of data
    move left according to pointer
    if n == 1
        we are in first space, find the next function to run (memory flag decode/load/fetch)
    if n == 2
        memory move left ( pointer = 128 ; n = 1 )

'''

# landing is _ _ f 1


def unpack_bits(origin, targets):
    assert len(targets) >= 9
    steps = set(targets[i+1] - targets[i] for i in range(len(targets)-1))
    assert len(steps) == 1
    step = list(steps)[0]
    # [->>[>]+<[-<]<]
    return loopdown(origin, at(targets[1],
        # step forward until zero
        loop(g(step))
        # set bit to 1
        + c(1)
        # step back until zero
        + g(-step) + loop(
            # clear bit to 0
            c(-1) + g(-step))
        # reset pointer
        + g(step)
    ))

def pack_bits(target, origins):  # Unused now since binary operations also pack in the same step
    assert len(origins) >= 9
    assert len(set(origins[i+1] - origins[i] for i in range(len(origins)-1))) == 1
    # >->>>>>>>+[->[<++>-]<<+]>[<<+>>-]<<
    return join(
        at(origins[0], c(-1)),
        loopuntil(-1, origins[-2], join(
            move(origins[-1], 2*[origins[-2]]),
            g(-origins[-2] + origins[-3]),
        )),
        g(-origins[0] + origins[-2]),
        at(origins[0], c(1)),
        move(origins[1], target),
    )
# print(pack_bits(0, range(1,10)))



class ReadWritePrefix(Struct):
    """Prefix struct for reading/writing to the right-adjacent cells"""
    DATA_SIZE = 4
    # type/width is implicitly encoded as follows:
    # word is xyzw1
    # half is xy1__
    # byte is x1___
    # read is _____
    data = Offset(range(0, DATA_SIZE))
    _t = Offset(DATA_SIZE)
    end = Offset(DATA_SIZE+1)
    memory = Offset(range(DATA_SIZE+1, DATA_SIZE+1+4))

# todo try optimizing code size (could have a zero to the left if needed)
class ReadWrite:
    rwp = ReadWritePrefix()
    test = init([
        6,2,1,0,0,  # data
        *b"abcdefghijkl",  # memory
    ])
    code = lambda rwp=rwp: join(
        # if the cell to the right is nonzero, write to memory.
        # repeat for all cells
        join(
            ifnonzero(rwp.data[i]+1, join(
                at(rwp.memory[i], s(0)),
                move(rwp.data[i], rwp.memory[i]),
                at(rwp.data[i], c(1)),
            ))
            for i in reversed(range(size(rwp.data)))
        ),
        # set rwp._t to 1 iff there are no writes
        at(rwp._t, c(1)),
        ifnonzero(rwp.data[0], at(rwp._t, c(-1))),
        ifnonzero(rwp._t, join(
            at(rwp._t, s(0)),
            # for all cells, read from memory
            join(
                move(rwp.memory[i], rwp._t)
                + move(rwp._t, [rwp.memory[i], rwp.data[i]])
                for i in range(size(rwp.data))
            ),
        )),
    )

@pack_class_offsets
class MemoryHeadData(Struct):
    # __fnIJKLxyzw_
    land = Offset(range(2))
    f = Offset()
    n = Offset()
    i = Offset(range(4))
    rwp = Offset(ReadWritePrefix())

@pack_class_offsets
class MemoryHead(Struct):
    """Moving head shifting through memory using an pointer"""
    # _____ijkl_data
    POINTER_SIZE = 4
    _t = Offset(range(POINTER_SIZE))
    decr_flag = Offset()
    i = Offset(range(POINTER_SIZE))
    _a = Offset()
    data = Offset(MemoryHeadData())


# _____ijkl___fnIJKLxyzw_
class MemoryMove:
    head = MemoryHead()
    test = init([
        0,0,0,0,
        0,
        5,0,0,0,  # i j k l
        0,
        1,2,3,4, 5,0,0,0, *[0]*5,  # data
        *b"abcdefghijkl",  # memory
    ])
    code = lambda head=head, rightward=True: join(
        at(head.decr_flag, c(1)),
        loopuntil(0, head.decr_flag, join(
            at(head.decr_flag, c(-1)),
            # check from lsb to msb
            join(
                ifnonzero(x, join(
                    join(
                        at(head.i[j], c(-1))  # subtract from lower bytes
                        for j in reversed(range(i+1))
                    ),
                    at(head.decr_flag, c(1)),
                    g(-head.i[0] + head._t[0]),
                ))
                for i, x in enumerate(head.i)
            ),
            # pointer is at head._a or head.decr_flag if a path above was taken
            ifnonzero(head._a, join(
                g(-head.decr_flag + head._a), # pointer is at head.decr_flag, offset to compensate
                at(head.decr_flag, c(-1)),
                (
                    join(
                        move(head[-1]+1, head[0]),
                        join(move(x, x+1) for x in reversed(shift(range(size(head.data)), start(head.data)))),
                        join(move(x, x+1) for x in reversed(head.i)),
                        g(1),
                    )
                    if rightward else
                    join(
                        join(move(x, x-1) for x in head.i),
                        join(move(x, x-1) for x in shift(range(size(head.data)), start(head.data))),
                        move(head[0]-1, head[-1]),
                        g(-1),
                    )
                ),
                at(head._t[-1], c(1)),
            )),
            # g(-head.decr_flag + head._a),  # OLD: pointer is at head._a or head.decr_flag if above was run
            move(head._t[-1], head.decr_flag),  # from setting head._t[-1] above
        )),
        # g(head.decr_flag - head._a),  # OLD: pointer is at head._a
    )

# print(MemoryMove.test)
# print(MemoryMove.code())
# print(ReadWrite.code(rwp=ReadWritePrefix(MemoryHead().data[8])))
# print(join(
    # move(d, i)
    # for i, d in zip(MemoryHead().i, MemoryHead().data[4:8])
# ))
# print(MemoryMove.code(rightward=False))


class InterleavedInt(Struct):
    """Structure containing two interleaved 4 byte ints"""
    # - aA1_ bB1_ cC1_ dD1_ ____
    _a = Offset(0)
    num1 = Offset(range(1, 1+5*4, 4))
    num2 = Offset(range(2, 2+5*4, 4))
    _t3 = Offset(range(3, 3+5*4, 4))
    _t4 = Offset(range(4, 4+5*4, 4))

    def negate2(self):
        # -> >>>> >>>> >>>> >>> +[-<< +[->-<]>[-<+>]<< <+]
        return at(self._a,
            c(-1) + g(-self._a + self._t4[3])
            + loopuntil(-1,
                at(-self._t4[3] + self.num2[3],
                    c(1)
                    + loopdown(at(-self.num2[3] + self._t3[3], c(-1)))
                    + move(-self.num2[3] + self._t3[3], 0)
                )
                + g(-self._t4[3] + self._t4[2])
            )
            + c(1)
        )

    # Calculate num1 + num2 and store result in num2.
    # Note that num1 - num2 is the same as num1 + (~num2 + 1).
    # Remember to clear the MSB after use!
    def add1to2(self):
        assert self._t3[0] - self.num2[0] == self._t4[0] - self._t3[0]
        return at(self._a,
            c(-1) + g(-self._a + self._t4[3])
            + loopuntil(-1,
                at(-self._t4[3] + self.num1[3],
                    loopdown(
                        at(-self.num1[3] + self._t3[3], c(1))
                        + at(-self.num1[3] + self.num2[3],
                            c(1)
                            # num2 is nonzero, no need to do anything extra
                            + loop(g(-self.num2[3] + self._t3[3]) + c(-1))
                            + g(-self.num2[3] + self._t3[3])
                            # num2 is zero, we overflowed this byte, try incrementing larger bytes
                            + loop(
                                loop(
                                    c(-1) + g(-self._t3[3] + self.num2[4])
                                    + at(-self.num2[4] + self._t3[4], c(2))
                                    + c(1)
                                    # num2 is nonzero, we stop moving to larger bytes
                                    + loop(g(-self.num2[4] + self._t3[4]) + c(-2))
                                    + g(-self.num2[4] + self._t3[4])
                                    # num2 is zero, t3 == 2, next iteration will make t3 = 1
                                )
                                # go back to original byte
                                + at(-self._t4[4] + self._t3[4],
                                    c(1)
                                    + loop(c(-1) + g(-self._t3[4] + self._t3[3]))
                                )
                            )
                            + g(-self._t4[3] + self.num2[3])
                        )
                    )
                )
                + g(-self._t4[4] + self._t4[3])
            )
            + c(1)
        )

    # Calculate num1 < num2 and store 1 in num2 if true, otherwise 0.
    # Note that only num2[0] can be used to check if the expression is true.
    def compare1lessthan2(self):
        assert self._t3[0] - self.num2[0] == self._t4[0] - self._t3[0]
        return at(self._a,
            c(-1) + g(-self._a + self._t4[3])
            + loopuntil(-1,
                at(-self._t4[3] + self.num1[3],
                    # TODO: make single byte comparison into separate function
                    # inspiration from
                    # https://www.codingame.com/playgrounds/50455/brainfuck-part-4---advanced-maths/compare-integers
                    loopdown(
                        at(-self.num1[3] + self._t3[3], c(1))
                        + at(-self.num1[3] + self.num2[3],
                            # num2 is nonzero, no need to do anything extra
                            loop(c(-1) + g(-self.num2[3] + self._t3[3]) + c(-1))
                            + g(-self.num2[3] + self._t3[3])
                            # num2 is zero, so 1 >= 2, clear num1
                            + loop(
                                at(-self._t3[3] + self.num1[3], s(0))
                                + g(-self._t3[3] + self._t4[3])
                            )
                            + g(-self._t4[3] + self.num2[3])
                        )
                    )
                    # if num2, then num1 < num2
                    # >[[-]<< +[- <<[-]<[-]< +]- >+>]
                    + at(-self.num1[3] + self.num2[3],
                        loop(
                            s(0)
                            + at(-self.num2[3] + self._t4[2],
                                loopuntil(-1,
                                    at(-self._t4[2] + self.num2[2], s(0))
                                    + at(-self._t4[2] + self.num1[2], s(0))
                                    + g(-self._t4[2] + self._t4[1])
                                )
                            )
                            + at(-self.num2[3] + self.num1[3], c(1))
                        )
                    )
                    # if t3, then num1 > num2
                    # >[-<<< +[- <<[-]<[-]< +]- >>>]
                    + at(-self.num1[3] + self._t3[3],
                        loop(
                            c(-1)
                            + at(-self._t3[3] + self._t4[2],
                                loopuntil(-1,
                                    at(-self._t4[2] + self.num2[2], s(0))
                                    + at(-self._t4[2] + self.num1[2], s(0))
                                    + g(-self._t4[2] + self._t4[1])
                                )
                            )
                        )
                    )
                    + move(-self.num1[3] + self.num2[3])
                )
                + g(-self._t4[4] + self._t4[3])
            )
            + c(1)
        )

class PairedBits(Struct):
    """Structure containing two interleaved 8 bit pointers"""
    # xy-_aAbB..hH__
    # unpacking inspired from
    # https://codegolf.stackexchange.com/questions/9178/bitwise-operators-in-brainfuck/15291#15291
    byte1 = Offset(0)
    byte2 = Offset(1)
    bits1 = Offset(range(2, 2+10*2, 2))
    bits2 = Offset(range(3, 2+10*2, 2))

    def _unpack1(self):
        return unpack_bits(self.byte1, self.bits1)

    def _unpack2(self):
        return unpack_bits(self.byte2, self.bits2)

    # Result is in byte1
    def bitwise_xor(self):
        # >>->>>>>>>>>>>>>>>>
        # go down each bit, xoring along the way
        # +[- [->+<]> [<+>-[<->[-]]] >[-<<++>>]<<<< +]-
        # +>>[-<<<<+>>>>]<<<<
        return self._bitwise_operation(lambda a, b: at(b, loop(c(-1) + at(-b+a, c(1)) + loop(s(0) + at(-b+a, c(-1))))))

    def bitwise_or(self):
        # >>->>>>>>>>>>>>>>>>
        # go down each bit, oring along the way
        # +[- [->+<]> [<+>[-]] >[-<<++>>]<<<< +]-
        # +>>[-<<<<+>>>>]<<<<
        return self._bitwise_operation(lambda a, b: at(b, loop(s(0) + at(-b+a, c(1)))))

    def bitwise_and(self):
        # >>->>>>>>>>>>>>>>>>
        # go down each bit, anding along the way
        # +[- [->+<]> [-[<+>[-]]] >[-<<++>>]<<<< +]-
        # +>>[-<<<<+>>>>]<<<<
        return self._bitwise_operation(lambda a, b: at(b, loop(c(-1) + loop(s(0) + at(-b+a, c(1))))))

    def _bitwise_operation(self, bit_func):
        return (
            self._unpack1()
            + self._unpack2()
            + at(self.bits1[0], c(-1))
            + g(self.bits1[8])
            + loopuntil(-1,
                move(-self.bits1[8] + self.bits2[8])
                + bit_func(0, -self.bits1[0] + self.bits2[0])
                + at(-self.bits1[8] + self.bits1[9], loopdown(at(-self.bits1[9] + self.bits1[8], c(2))))
                + g(-self.bits1[8] + self.bits1[7])
            )
            + c(1)
            + g(-self.bits1[0])
            + move(self.bits1[1], self.byte1)
        )

def compare1equal2(num1, num2):
    # abcdABCD
    assert len(num1) == len(num2)
    assert len(num1) > 1
    return (
        # [->>>>-<<<<]> [->>>>-<<<<]> [->>>>-<<<<]> [->>>>-<<<<]>
        "".join(
            at(byte1, loopdown(at(-byte1 + byte2, c(-1))))
            for byte1, byte2 in zip(num1, num2)
        )
        # [[-]>[-]+<]> [[-]>[-]+<]> [[-]>[-]+<]>
        + "".join(
            at(num2[i], ifnonzero(at(-num2[i] + num2[i-1], s(1))))
            for i in reversed(range(1, len(num2)))
        )
        # [[-] <<< <<<< - >>>> >>>]
        + at(num2[0], ifnonzero(at(-num2[0] + num1[0], c(-1))))
        # <<< <<<< +
        + at(num1[0], c(1))
    )
    # print(compare1equal2(range(4), range(4,8)))

# print(","+InterleavedInt().negate2())

# print(
    # at(2, s(2)),
    # move(2, 4),
    # switch(
        # "".join(s(x)+"." for x in b'default') + move(4, 1),
        # [2, "".join(s(x)+"." for x in b'two') + move(4, 1)],
        # [6, "".join(s(x)+"." for x in b'six') + move(4, 1)],
        # [4, "".join(s(x)+"." for x in b'four') + move(4, 1)],
        # on=4,
    # ),
# )



@mark_function
def DECODE():
    args = DECODE_ARGS()
    code = ""
    code += DECODE_OPCODE_TYPE(*args)
    code += DECODE_INSTR_PARTS(*args)
    code += EXECUTE(*args)
    # code += at(DECODE_FORMAT().F, s(ERROR.f))
    return code

@mark_function
def INCPC():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        zipmove(h.pc, h.rs2),  # pc.. => rs2.
        at(h.rs1[0], s(4)),  # rs1. = 4
        at(h.F, s(ADD.f)),  # F = ADD
        at(h.N, s(_INCPC_1.f)),  # N = ENDCYCLE
    )

@mark_function
def _INCPC_1():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        # update pc from rs2
        zipmove(h.rs2, h.pc),
        # fetch next instruction
        at(h.F, s(FETCH.f)),
    )


#                                               46   51      59
#                                          _____ijkl___fnIJKLxyzw_
#                                         40  44
#                         24              M   S12dimm.
#                         rs1.rs2.rd..pc..__FN
# ___-aA1_bB1_cC1_dD1_____
# xy-_aAbBcCdDeEfFgGhH__
#                         abcdABCD
def DECODE_FORMAT():
    # BB, F, N, xyzw, S, x1, x2, xd, imm
    # maps onto
    # BB, F, N, mem.data.rwp.data, funct_flag, x1, x2, xd, imm
    class RiscvHead(Struct):
        """Structure containing working space for the RISC-V emulator"""
        mem_next = Offset(40)
        mem = Offset(MemoryHead(41))
        funct_flag = Offset(44)
        x1 = Offset(45)
        x2 = Offset(46)
        xd = Offset(47)
        imm = Offset(range(48,48+4))

        rs1 = Offset(range(24,24+4))
        rs2 = Offset(range(28,28+4))
        rd = Offset(range(32,32+4))
        pc = Offset(range(36,36+4))
        BB = Offset(range(27,27+9))
        B = Offset(BB.offset[1:])

        land = Offset(range(40,40+2))
        F = Offset(42)
        N = Offset(43)

        ints = Offset(InterleavedInt(3))
        bits = Offset(PairedBits(0))

        end = Offset(64)

    return RiscvHead()

def DECODE_ARGS():
    rhead = DECODE_FORMAT()
    POS = dict(
        BB = rhead.BB,
        F = rhead.F,
        N = rhead.N,
        xyzw = rhead.mem.data.rwp.data,
        S = rhead.funct_flag,
        x1 = rhead.x1,
        x2 = rhead.x2,
        xd = rhead.xd,
        imm = rhead.imm,
    )
    pos = lambda name: POS[name]
    return [pos(name.strip()) for name in "BB, F, N, xyzw, S, x1, x2, xd, imm".split(",")]

def DECODE_OPCODE_TYPE(BB, F, N, xyzw, S, x1, x2, xd, imm):
    assert size(BB) == 9
    assert size(F) == 1
    assert size(N) == 1
    assert size(xyzw) == 4
    assert size(S) == 1
    assert size(x1) == 1
    assert size(x2) == 1
    assert size(xd) == 1
    assert size(imm) == 4
    B = BB[1:]
    INFO_BITS = lambda *bits: "".join(move(B[i], 2*[B[i-1]]) for i in reversed(range(8)) if i not in bits)
    return join(
        # unpack first byte to determine opcode / fmt (RISBUJ)
        # [-1] = .rs2.rd..
        # unpack_bits(x, [])
        unpack_bits(xyzw[0], BB),
        # if [1]: [1] = 0; [0] += 2
        # if [3]: [3] = 0; [2] += 2
        # { if [6]: [6] = 0; [5] += 2 }; { if [5]: [5] = 0; [4] += 2 }
        INFO_BITS(0, 2, 4, 7),  # 0, 2, 4, 7 have info
        at(F, s(0)), move(B[0], F),  # F = 0; [0] => F

        # [1:0]
        switch(on=F)(  # switch F:
            # default:
            join(
                at(F, s(ERROR.f)),  # F = ERROR
                at(N, s(E_INVALID_OPCODE_1)),
                at(B, s(0)),
                g(-2),  # land __FN
            ),
            # case 3:  # all RV32I instructions have 11 as their lowest two bits
            [3, join(
                at(F, s(0)),
                move(B[2], F),  # [2] => F

                # [3:2]
                switch(on=F)(  # switch F:
                    # default:
                    join(
                        at(F, s(ERROR.f)),  # F = ERROR
                        at(N, s(E_INVALID_OPCODE_2)),
                        at(B, s(0)),
                        g(-2),  # land __FN
                    ),

                    # case 0:  # most instructions
                    join(
                        at(F, s(0)),
                        move(B[4], F),  # [4] => F
                        # maybe switch to the [-[-[- trick?
                        switch(on=F)(  # switch F:
                            # default:
                            join(
                                at(F, s(ERROR.f)),  # F = ERROR
                                at(N, s(E_INVALID_OPCODE_3)),
                                g(-2),  # land __FN
                            ),
                            # case 0:
                            join(
                                # I type
                                # lb (0), lh (1), lw (2), lbu (4), lhu (5)
                                at(F, s(1)),  # F = 1
                                at(N, s(1)),  # N = 1
                                g(-2),  # land __FN
                                # Note that N starts at 1 since we are using N=0 as an error in the next step
                            ),
                            # case 1:
                            join(
                                # I type
                                # addi (0), slli (1), slti (2), sltiu (3), xori (4), srli/srai (5), ori (6), andi (7)
                                at(F, s(1)),  # F = 1
                                at(N, s(2)),  # N = 2
                                at(x2, s(1)),  # 2 = 1
                                g(-2),  # land __FN
                            ),
                            # case 2:
                            join(
                                # S type
                                # sb (0), sh (1), sw (2)
                                at(F, s(2)),  # F = 2
                                at(N, s(3)),  # N = 3
                                g(-2),  # land __FN
                            ),
                            # case 3:
                            join(
                                # R type
                                # add/sub (0), sll (1), slt (2), sltu (3), xor (4), srl/sra (5), or (6), and (7)
                                at(F, s(0)),  # F = 0
                                at(N, s(4)),  # N = 4
                                at(imm[0], s(1)),
                                g(-2),  # land __FN
                            ),
                            # case 6:
                            [6, join(
                                # B type
                                # beq (0), bne (1), blt (4), bge (5), bltu (6), bgeu (7)
                                at(F, s(3)),  # F = 3
                                at(N, s(5)),  # N = 5
                                g(-2),  # land __FN
                            )],
                            # case 7:
                            join(
                                # R type
                                # ecall (0), ebreak (1)
                                # Note: Even though ECALL and EBREAK look like
                                # I-type instructions, it is part of the SYSTEM
                                # family of opcodes where using R-type would
                                # aid in decoding the instruction.
                                at(F, s(0)),  # F = 0
                                at(N, s(6)),  # N = 6
                                g(-2),  # land __FN
                            ),
                        ),
                    ),

                    # case 1:  # lui (011), auipc (001), jalr (110)
                    join(
                        at(F, s(0)),
                        move(B[4], F),  # [4] => F
                        switch(on=F)(  # switch F:
                            # default:
                            join(
                                at(F, s(ERROR.f)),  # F = ERROR
                                at(N, s(E_INVALID_OPCODE_4)),
                                g(-2),  # land __FN
                            ),
                            # case 1:
                            [1, join(
                                # U type
                                # auipc
                                at(F, s(4)),  # F = 4
                                at(N, s(7)),  # N = 7
                                g(-2),  # land __FN
                            )],
                            # case 3:
                            [3, join(
                                # U type
                                # lui
                                at(F, s(4)),  # F = 4
                                at(N, s(8)),  # N = 8
                                g(-2),  # land __FN
                            )],
                            # case 6:
                            [6, join(
                                # I type
                                # jalr
                                at(F, s(1)),  # F = 1
                                at(N, s(9)),  # N = 9
                                g(-2),  # land __FN
                            )],
                        ),
                    ),

                    # case 3:  # fence(0), jal (110)
                    [3, join(
                        at(F, s(0)),
                        move(B[4], F),  # [4] => F
                        # switch F:
                        switch(on=F)(
                            # default:
                            join(
                                at(F, s(ERROR.f)),  # F = ERROR
                                at(N, s(E_INVALID_OPCODE_5)),
                                g(-2),  # land __FN
                            ),
                            # case 0:
                            join(
                                # U type (its the cheapest path since we ignore all fences)
                                # fence
                                at(F, s(1)),  # F = 1
                                at(N, s(11)),  # N = 11
                                g(-2),  # land __FN
                            ),
                            # case 6:
                            [6, join(
                                # J type
                                # jal
                                at(F, s(5)),  # F = 5
                                at(N, s(10)),  # N = 10
                                g(-2),  # land __FN
                            )],
                        ),
                    )],
                ),
            )],
        ),
        g(+2),
    )

def DECODE_INSTR_PARTS(BB, F, N, xyzw, S, x1, x2, xd, imm):
    assert size(BB) == 9
    assert size(F) == 1
    assert size(N) == 1
    assert size(xyzw) == 4
    assert size(S) == 1
    assert size(x1) == 1
    assert size(x2) == 1
    assert size(xd) == 1
    assert size(imm) == 4
    B = BB[1:]
    INFO_BITS = lambda *bits: "".join(move(B[i], 2*[B[i-1]]) for i in reversed(range(8)) if i not in bits)
    # extract parts from instruction according to format
    # 012345 == RISBUJ
    return switch(on=F)(  # switch F:
        # default:
        join(
            # Keep N propagated
            at(F, s(E_DECODE_MISSING_ERRNO)),
            ifnonzero(N, at(F, s(0)) + move(N, F)),
            move(F, N),
            at(F, s(ERROR.f)),  # F = ERROR
            at(B, s(0)),
            g(-2),  # land __FN
        ),
        # case 0:  # R type
        join(
            move(B[7], xd), # [7] => d

            # unpack_bits(y, [])
            unpack_bits(xyzw[1], BB),
            # { if [3]: [3] = 0; [2] += 2 }; { if [2]: [2] = 0; [1] += 2 }; { if [1]: [1] = 0; [0] += 2 }
            # { if [6]: [6] = 0; [5] += 2 }; { if [5]: [5] = 0; [4] += 2 }
            INFO_BITS(0, 4, 7),  # 0, 4, 7 have info
            move(B[0], 2*[xd]),  # while [0]: [0] -= 1; d += 2
            move(B[4], S),  # [4] => S
            move(B[7], x1),  # [7] => 1

            # unpack_bits(z, [])
            unpack_bits(xyzw[2], BB),
            # { if [3]: [3] = 0; [2] += 2 }; { if [2]: [2] = 0; [1] += 2 }; { if [1]: [1] = 0; [0] += 2 }
            # { if [7]: [7] = 0; [6] += 2 }; { if [6]: [6] = 0; [5] += 2 }; { if [5]: [5] = 0; [4] += 2 }
            INFO_BITS(0, 4),  # 0, 4 have info
            move(B[0], 2*[x1]),  # while [0]: [0] -= 1; 1 += 2
            move(B[4], x2),  # [4] => 2

            # unpack_bits(w, [])
            unpack_bits(xyzw[3], BB),
            # { if [7]: [7] = 0; [5] += 2 }; { if [5]: [5] = 0; [4] += 2 }; { if [4]: [4] = 0; [3] += 2 }; { if [3]: [3] = 0; [2] += 2 }; { if [2]: [2] = 0; [1] += 2 }
            INFO_BITS(0, 1, 6, 7),  # 0, 1, 6 have info
            move(B[0], 16*[x2]),  # if [0]: [0] = 0; 2 += 16
            # ensure all other bits are 0
            ifnonzero(imm[0], join(
                ifnonzero(B[6], at(S, c(8))),  # if [6]: [6] = 0; S += 8
                ifnonzero(B[1], at(N, s(0))),  # if [1]: [1] = 0; N = ERROR
                ifnonzero(B[7], at(N, s(0))),  # if [1]: [1] = 0; N = ERROR
            )),
            move(B[7], 2*[B[6]]),
            move(B[6], 4*[B[4]]),
            move(B[4], 8*[B[1]]),
            move(B[1], imm[0]),

            # S=funct3+((funct7&0x20)<<3) 1=rs1 2=rs2 d=rd
            at(F, s(ERROR.f)), at(F-1, s(E_INVALID_FUNCT7_MATH_R)), ifnonzero(N, at(F, s(0)) + move(N, F) + at(F-1, s(0))), move(F-1, N),  # N => F
            g(-2),  # land __FN
        ),

        # case 1:  # I type
        join(
            move(B[7], xd),  # [7] => d

            # unpack_bits(y, [])
            unpack_bits(xyzw[1], BB),
            # { if [3]: [3] = 0; [2] += 2 }; { if [2]: [2] = 0; [1] += 2 }; { if [1]: [1] = 0; [0] += 2 }
            # { if [6]: [6] = 0; [5] += 2 }; { if [5]: [5] = 0; [4] += 2 }
            INFO_BITS(0, 4, 7),  # 0, 4, 7 have info
            move(B[0], 2*[xd]),  # while [0]: [0] -= 1; d += 2
            move(B[4], [S, x1]),  # [4] => S + 1
            # at 1: [2+1-[2-1-[-[-[2+1-[2-1[-]]]]]]]  # 2+=1 iff S=1or5
            loopdown(x1, at(x2, c(1)) +
            loopdown(x1, at(x2, c(-1)) +
            loopdown(x1,
            loopdown(x1,
            loopdown(x1, at(x2, c(1)) +
            loopdown(x1, at(x2, c(-1)) +
            at(x1, s(0))
            )))))),
            # at 2: [-[1+2[-]]]  # 1=1 iff 2=2
            loopdown(x2,
            loopdown(x2, at(x1, c(1)) +
            at(x2, s(0))
            )),
            move(x1, x2),  # 1 => 2
            move(B[7], x1),  # [7] => 1

            # unpack_bits(z, [])
            unpack_bits(xyzw[2], BB),
            # { if [3]: [3] = 0; [2] += 2 }; { if [2]: [2] = 0; [1] += 2 }; { if [1]: [1] = 0; [0] += 2 }
            # { if [7]: [7] = 0; [6] += 2 }; { if [6]: [6] = 0; [5] += 2 }; { if [5]: [5] = 0; [4] += 2 }
            INFO_BITS(0, 4),  # 0, 4 have info
            move(B[0], 2*[x1]),  # while [0]: [0] -= 1; 1 += 2
            move(B[4], imm[0]),  # [4] => imm.[0]

            # unpack_bits(w, [])
            unpack_bits(xyzw[3], BB),
            # { if [3]: [3] = 0; [2] += 2 }; { if [2]: [2] = 0; [1] += 2 }; { if [1]: [1] = 0; [0] += 2 }
            # if [5]: [5] = 0; [4] += 2
            INFO_BITS(0, 1, 4, 6, 7),  # 0, 1, 4, 6, 7 have info
            ifnonzero(x2, join(  # if 2:
                # 2 = 0
                ifnonzero(B[6], at(S, c(8))),  # if [6]: [6] = 0; S += 8
                ifnonzero(B[1], at(N, s(0))),  # if [1]: [1] = 0; N = ERROR
                ifnonzero(B[4], at(N, s(0))),  # if [4]: [4] = 0; N = ERROR
                ifnonzero(B[7], at(N, s(0))),  # if [7]: [7] = 0; N = ERROR
            )),
            move(B[1], 2*[B[0]]),  # while [1]: [1] -= 1; [0] += 2
            move(B[0], 16*[imm[0]]),  # while [0]: [0] -= 1; imm.[0] += 16
            move(B[6], 4*[B[4]]),  # if [6]: [6] = 0; [4] += 4
            move(B[4], imm[1]),  # [4] => imm.[1]
            ifnonzero(B[7], join(  # if [7]:
                # [7] = 0
                at(imm[1], c(-8)),  # imm.[1] += 0b11111000  # probably faster to subtract 8
                at(imm[2], c(-1)),  # imm.[2] += 0b11111111
                at(imm[3], c(-1)),  # imm.[3] += 0b11111111
            )),

            # S=funct3+((funct7&0x20)<<3) 1=rs1 d=rd imm.=imm(sign extended iff not slli/srli/srai)
            at(F, s(ERROR.f)), at(F-1, s(E_INVALID_FUNCT7_MATH_I)), ifnonzero(N, at(F, s(0)) + move(N, F) + at(F-1, s(0))), move(F-1, N),  # N => F
            g(-2),  # land __FN
        ),

        # case 2:  # S type
        join(
            move(B[7], imm[0]),  # [7] => imm.[0]

            # unpack_bits(y, [])
            unpack_bits(xyzw[1], BB),
            # { if [3]: [3] = 0; [2] += 2 }; { if [2]: [2] = 0; [1] += 2 }; { if [1]: [1] = 0; [0] += 2 }
            # { if [6]: [6] = 0; [5] += 2 }; { if [5]: [5] = 0; [4] += 2 }
            INFO_BITS(0, 4, 7),  # 0, 4, 7 have info
            move(B[0], 2*[imm[0]]),  # while [0]: [0] -= 1; imm.[0] += 2
            move(B[4], S),  # [4] => S
            move(B[7], x1),  # [7] => 1

            # unpack_bits(z, [])
            unpack_bits(xyzw[2], BB),
            # { if [3]: [3] = 0; [2] += 2 }; { if [2]: [2] = 0; [1] += 2 }; { if [1]: [1] = 0; [0] += 2 }
            # { if [7]: [7] = 0; [6] += 2 }; { if [6]: [6] = 0; [5] += 2 }; { if [5]: [5] = 0; [4] += 2 }
            INFO_BITS(0, 4),  # 0, 4 have info
            move(B[0], 2*[x1]),  # while [0]: [0] -= 1; 1 += 2
            move(B[4], x2),  # [4] => 2

            # unpack_bits(w, [])
            unpack_bits(xyzw[3], BB),
            # { if [3]: [3] = 0; [2] += 2 }; { if [2]: [2] = 0; [1] += 2 }
            # { if [6]: [6] = 0; [5] += 2 }; { if [5]: [5] = 0; [4] += 2 }
            INFO_BITS(0, 1, 4, 7),  # 0, 1, 4, 7 have info
            move(B[0], 16*[x2]),  # if [0]: [0] = 0; 2 += 16
            move(B[1], 32*[imm[0]]),  # while [1]: [1] -= 1; imm.[0] += 32  # TODO: shorten?
            move(B[4], imm[1]),  # [4] => imm.[1]
            ifnonzero(B[7], join(  # if [7]:
                # [7] = 0
                at(imm[1], c(-8)),  # imm.[1] += 0b11111000
                at(imm[2], c(-1)),  # imm.[2] += 0b11111111
                at(imm[3], c(-1)),  # imm.[3] += 0b11111111
            )),

            # S=funct3 1=rs1 2=rs2 imm.=imm(sign extended)
            at(F, s(0)), move(N, F),  # N => F
            g(-2),  # land __FN
        ),

        # case 3:  # B type
        join(
            move(B[7], 8*[imm[1]]),  # if [7]: [7] = 0; imm.[1] += 8

            # unpack_bits(y, [])
            unpack_bits(xyzw[1], BB),
            # { if [3]: [3] = 0; [2] += 2 }; { if [2]: [2] = 0; [1] += 2 }; { if [1]: [1] = 0; [0] += 2 }
            # { if [6]: [6] = 0; [5] += 2 }; { if [5]: [5] = 0; [4] += 2 }
            INFO_BITS(0, 4, 7),  # 0, 4, 7 have info
            move(B[0], 2*[imm[0]]),  # while [0]: [0] -= 1; imm.[0] += 2
            move(B[4], S),  # [4] => S
            move(B[7], x1),  # [7] => 1

            # unpack_bits(z, [])
            unpack_bits(xyzw[2], BB),
            # { if [3]: [3] = 0; [2] += 2 }; { if [2]: [2] = 0; [1] += 2 }; { if [1]: [1] = 0; [0] += 2 }
            # { if [7]: [7] = 0; [6] += 2 }; { if [6]: [6] = 0; [5] += 2 }; { if [5]: [5] = 0; [4] += 2 }
            INFO_BITS(0, 4),  # 0, 4 have info
            move(B[0], 2*[x1]),  # while [0]: [0] -= 1; 1 += 2
            move(B[4], x2),  # [4] => 2

            # unpack_bits(w, [])
            unpack_bits(xyzw[3], BB),
            # { if [3]: [3] = 0; [2] += 2 }; { if [2]: [2] = 0; [1] += 2 }
            # { if [6]: [6] = 0; [5] += 2 }; { if [5]: [5] = 0; [4] += 2 }
            INFO_BITS(0, 1, 4, 7),  # 0, 1, 4, 7 have info
            move(B[0], 16*[x2]),  # if [0]: [0] = 0; 2 += 16
            move(B[1], 32*[imm[0]]),  # while [1]: [1] -= 1; imm.[0] += 32
            move(B[4], imm[1]),  # [4] => imm.[1]
            ifnonzero(B[7], join(  # if [7]:
                # [7] = 0
                at(imm[1], c(-16)),  # imm.[1] += 0b11110000  # note extra 0
                at(imm[2], c(-1)),  # imm.[2] += 0b11111111
                at(imm[3], c(-1)),  # imm.[3] += 0b11111111
            )),

            # S=funct3 1=rs1 2=rs2 imm.=imm(sign extended)
            at(F, s(0)), move(N, F),  # N => F
            g(-2),  # land __FN
        ),

        # case 4:  # U type
        join(
            move(B[7], xd),  # [7] => d

            # unpack_bits(y, [])
            unpack_bits(xyzw[1], BB),
            # { if [3]: [3] = 0; [2] += 2 }; { if [2]: [2] = 0; [1] += 2 }; { if [1]: [1] = 0; [0] += 2 }
            # { if [7]: [7] = 0; [6] += 2 }; { if [6]: [6] = 0; [5] += 2 }; { if [5]: [5] = 0; [4] += 2 }
            INFO_BITS(0, 4),  # 0, 4 have info
            move(B[0], 2*[xd]),  # while [0]: [0] -= 1; d += 2
            move(B[4], 16*[imm[1]]),  # while [4]: [4] -= 1; imm.[1] += 16

            move(xyzw[2], imm[2]),  # z => imm.[2]
            move(xyzw[3], imm[3]),  # w => imm.[3]

            # d=rd imm.=imm
            at(F, s(0)), move(N, F),  # N => F
            g(-2),  # land __FN
        ),

        # case 5:  # J type
        join(
            move(B[7], xd),  # [7] => d

            # unpack_bits(y, [])
            unpack_bits(xyzw[1], BB),
            # { if [3]: [3] = 0; [2] += 2 }; { if [2]: [2] = 0; [1] += 2 }; { if [1]: [1] = 0; [0] += 2 }
            # { if [7]: [7] = 0; [6] += 2 }; { if [6]: [6] = 0; [5] += 2 }; { if [5]: [5] = 0; [4] += 2 }
            INFO_BITS(0, 4),  # 0, 4 have info
            move(B[0], 2*[xd]),  # while [0]: [0] -= 1; d += 2
            move(B[4], 16*[imm[1]]),  # while [4]: [4] -= 1; imm.[1] += 16

            # unpack_bits(z, [])
            unpack_bits(xyzw[2], BB),
            # { if [3]: [3] = 0; [2] += 2 }; { if [2]: [2] = 0; [1] += 2 }; { if [1]: [1] = 0; [0] += 2 }
            # { if [7]: [7] = 0; [6] += 2 }; { if [6]: [6] = 0; [5] += 2 }
            INFO_BITS(0, 4, 5),  # 0, 4, 5 have info
            move(B[0], imm[2]),  # [0] => imm.[2]
            move(B[4], 8*[imm[1]]),  # if [4]: [4] = 0; imm.[1] += 8
            move(B[5], 2*[imm[0]]),  # while [5]: [5] -= 1; imm.[0] += 2

            # unpack_bits(w, [])
            unpack_bits(xyzw[3], BB),
            # { if [3]: [3] = 0; [2] += 2 }; { if [2]: [2] = 0; [1] += 2 }; { if [1]: [1] = 0; [0] += 2 }
            # { if [6]: [6] = 0; [5] += 2 }; { if [5]: [5] = 0; [4] += 2 }
            INFO_BITS(0, 4, 7),  # 0, 4, 7 have info
            move(B[0], 16*[imm[0]]),  # while [0]: [0] -= 1; imm.[0] += 16
            move(B[4], imm[1]),  # [4] => imm.[1]
            ifnonzero(B[7], join(  # if [7]:
                # [7] = 0
                at(imm[2], c(-16)),  # imm.[2] += 0b11110000
                at(imm[3], c(-1)),  # imm.[3] += 0b11111111
            )),

            # d=rd imm.=imm(sign extended)
            at(F, s(0)), move(N, F),  # N => F
            g(-2),  # land __FN
        ),
    ) + g(+2)

# MEMMOVRIGHT = 1
# _LOAD_1 = 2

def zipmove(origin, target):
    origin = list(origin)
    target = list(target)
    assert len(origin) == len(target)
    return "".join(move(o, t) for o, t in zip(origin, target))

_LOAD_1 = mark_function("_LOAD_1")
_STORE_1 = mark_function("_STORE_1")
_ADDI_1 = mark_function("_ADDI_1")
_SLLI_1 = mark_function("_SLLI_1")
_XORI_1 = mark_function("_XORI_1")
_ORI_1 = mark_function("_ORI_1")
_ANDI_1 = mark_function("_ANDI_1")
_SLTIU_1 = mark_function("_SLTIU_1")
_SRLI_1 = mark_function("_SRLI_1")
_AUIPC_1 = mark_function("_AUIPC_1")
_JALR_1 = mark_function("_JALR_1")
_JAL_1 = mark_function("_JAL_1")
_BRANCH_1 = mark_function("_BRANCH_1")
_ADD_1 = mark_function("_ADD_1")
_SUB_1 = mark_function("_SUB_1")
_XOR_1 = mark_function("_XOR_1")
_OR_1 = mark_function("_OR_1")
_AND_1 = mark_function("_AND_1")
_SLTU_1 = mark_function("_SLTU_1")
_SLL_1 = mark_function("_SLL_1")
_SRL_1 = mark_function("_SRL_1")

def ON_FENCE(): h = DECODE_FORMAT(); return at(h.F, s(ERROR.f)) + at(h.N, s(E_FENCE))
def ON_SYSTEM(): h = DECODE_FORMAT(); return at(h.F, s(ERROR.f)) + at(h.N, s(E_SYSTEM))

def EXECUTE(BB, F, N, xyzw, S, x1, x2, xd, imm):
    assert size(BB) == 9
    assert size(F) == 1
    assert size(N) == 1
    assert size(xyzw) == 4
    assert size(S) == 1
    assert size(x1) == 1
    assert size(x2) == 1
    assert size(xd) == 1
    assert size(imm) == 4
    B = BB[1:]
    INFO_BITS = lambda *bits: "".join(move(B[i], 2*[B[i-1]]) for i in reversed(range(8)) if i not in bits)
    h = DECODE_FORMAT()
    md = h.mem.data
    # do the actual instruction
    return switch(on=F)(  # switch F:
        # default:
        join(
            # F = ERROR
            at(F, s(ERROR.f)),
            # break out of main loop?
            # maybe later have a trap
            g(-2),
        ),

        # case 1:
        [1, join(
            # I type
            # lb (0), lh (1), lw (2), lbu (4), lhu (5)
            # S x1 xd imm.

            # Pseudocode:
            # rd = M[rs1+imm]
            # get x1
            # add rs1+imm
            # get rs1+imm
            # store rd

            at(F, s(0)),
            zipmove(imm, h.rs1),  # imm. => rs1.
            move(S, h.rd[0]),  # S => rd..[0]
            move(xd, h.rd[1]),  # d => rd..[1]
            move(x1, 4*[h.mem.i[0], md.i[0]]),  # while 1: 1 -= 1; ijkl[0] += 4; IJKL[0] += 4
            at(md.f, s(MEMMOVRIGHT.f)),  # f = MEMMOVRIGHT
            at(md.n, s(1)),  # n = 1
            at(h.mem_next, s(_LOAD_1.f)),  # M = _LOAD_1
            g(-F + md.f - 2),  # land __fn
        )],

        # *0*r'''
        # REMEMBER TO CLEAR F IF YOU'RE MOVING
        [2, join(  # case 2:
            # I type
            # addi (0), slli (1), slti (2), sltiu (3), xori (4), srli (5), ori (6), andi (7)
            # srai (13)
            # S x1 xd imm.

            # switch on func3 (with default case)
            at(h.F, s(0)),
            move(h.funct_flag, h.F),
            switch(on=h.F)(
                join(
                    at(h.F, s(ERROR.f)),
                    at(h.N, s(E_INVALID_FUNCT3_MATH)),
                    g(-2),
                ),
                join(
                    # addi
                    at(h.F, s(0)),
                    zipmove(h.imm, h.rs1),
                    move(h.xd, h.rd[0]),
                    move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_ADDI_1.f)),
                    g(-h.F + md.f - 2),
                ),
                join(
                    # slli
                    # guaranteed 0 <= imm <= 31
                    # fuck it we loop down on it
                    at(h.F, s(0)),
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(-7)) + at(h.imm[2], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(-7)) + at(h.imm[2], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(-7)) + at(h.imm[2], c(1)) +
                    move(h.imm[0], h.imm[1])
                    ))))))))
                    ))))))))
                    )))))))),
                    # imm[1] has bitshift
                    # imm[2] has byteshift
                    move(h.xd, h.rd[0]),
                    move(h.imm[1], h.rd[1]),
                    move(h.imm[2], h.rd[2]),
                    move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_SLLI_1.f)),
                    g(-h.F + md.f - 2),
                ),
                join(
                    # slti signed
                    at(h.F, s(0)),
                    # Add 128 to msb of BOTH rs1 and imm
                    at(h.rs1[2], s(-2)),
                    loop(h.rs1[2], at(h.rs1[2], c(-2)) + at([h.rs1[3], h.rs2[3]], c(1))),
                    at([h.rs1[3], h.rs2[3]], c(1)),
                    zipmove(h.imm, h.rs2),
                    move(h.xd, h.rd[0]),
                    move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_SLTIU_1.f)),
                    g(-h.F + md.f - 2),
                ),
                join(
                    # slti
                    at(h.F, s(0)),
                    zipmove(h.imm, h.rs2),
                    move(h.xd, h.rd[0]),
                    move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_SLTIU_1.f)),
                    g(-h.F + md.f - 2),
                ),
                [4, join(
                    # xori
                    at(h.F, s(0)),
                    zipmove(h.imm, h.rs1),
                    move(h.xd, h.rd[0]),
                    move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_XORI_1.f)),
                    g(-h.F + md.f - 2),
                )],
                [5, join(
                    # srli (13 for srai)
                    # guaranteed 0 <= imm <= 31
                    at(h.F, s(0)),
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(-7)) + at(h.imm[2], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(-7)) + at(h.imm[2], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(-7)) + at(h.imm[2], c(1)) +
                    move(h.imm[0], h.imm[1])
                    ))))))))
                    ))))))))
                    )))))))),
                    # imm[1] has bitshift
                    # imm[2] has byteshift
                    # rd[3] has signed have another bit to the left of the 1111 and check for that when shifting left
                    move(h.xd, h.rd[0]),
                    move(h.imm[1], h.rd[1]),
                    move(h.imm[2], h.rd[2]),
                    move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_SRLI_1.f)),
                    g(-h.F + md.f - 2),
                )],
                [6, join(
                    # ori
                    at(h.F, s(0)),
                    zipmove(h.imm, h.rs1),
                    move(h.xd, h.rd[0]),
                    move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_ORI_1.f)),
                    g(-h.F + md.f - 2),
                )],
                [7, join(
                    # andi
                    at(h.F, s(0)),
                    zipmove(h.imm, h.rs1),
                    move(h.xd, h.rd[0]),
                    move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_ANDI_1.f)),
                    g(-h.F + md.f - 2),
                )],
                [13, join(
                    # srai (5 for srli)
                    # guaranteed 0 <= imm <= 31
                    at(h.F, s(0)),
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(-7)) + at(h.imm[2], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(-7)) + at(h.imm[2], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(1)) +
                    loopdown(h.imm[0], at(h.imm[1], c(-7)) + at(h.imm[2], c(1)) +
                    move(h.imm[0], h.imm[1])
                    ))))))))
                    ))))))))
                    )))))))),
                    # imm[1] has bitshift
                    # imm[2] has byteshift
                    # rd[3] has signed have another bit to the left of the 1111 and check for that when shifting left
                    move(h.xd, h.rd[0]),
                    move(h.imm[1], h.rd[1]),
                    move(h.imm[2], h.rd[2]),
                    at(h.rd[3], s(1)),
                    move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_SRLI_1.f)),
                    g(-h.F + md.f - 2),
                )],
            ),
        )],
        # ''',

        [3, join(  # case 3:
            # S type
            # sb (0), sh (1), sw (2)
            # S x1 x2 imm.

            # Pseudocode:
            # M[rs1+imm] = rs2
            # get x1
            # add rs1+imm
            # get x2
            # store rs2

            at(F, s(0)),
            zipmove(imm, h.rs1),  # imm. => rs1.
            move(S, h.rd[0]),  # S => rd..[0]
            move(x2, h.rd[1]),  # 2 => rd..[1]
            move(x1, 4*[h.mem.i[0], md.i[0]]),  # while 1: 1 -= 1; ijkl[0] += 4; IJKL[0] += 4
            at(md.f, s(MEMMOVRIGHT.f)),  # f = MEMMOVRIGHT
            at(md.n, s(1)),  # n = 1
            at(h.mem_next, s(_STORE_1.f)),  # M = _STORE_1
            g(-F + md.f - 2),  # land __fn
        )],

        # *0*r'''

        [4, join(  # case 4:
            # R type
            # add (0), sll (1), slt (2), sltu (3), xor (4), srl (5), or (6), and (7)
            # sub (8), sra(13)
            # S x1 x2 xd
            at(h.F, s(0)),
            move(h.funct_flag, h.F),
            switch(on=h.F)(  # switch on funct3
                join(
                    at(h.F, s(ERROR.f)),
                    at(h.N, s(E_INVALID_FUNCT3_MATH)),
                    g(-2),
                ),
                join(
                    # add
                    # load x2 into rs1
                    # load x1 into rs2
                    # call add
                    # store rs2 into xd
                    at(h.F, s(0)),
                    move(h.xd, h.rd[0]),
                    move(h.x1, h.rd[1]),
                    move(h.x2, h.x1), move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_ADD_1.f)),
                    g(-h.F + md.f - 2),
                ),
                join(
                    # sll
                    at(h.F, s(0)),
                    move(h.xd, h.rd[0]),
                    move(h.x1, h.rd[1]),
                    move(h.x2, h.x1), move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_SLL_1.f)),
                    g(-h.F + md.f - 2),
                ),
                [2, join(
                    # slt signed
                    at(h.F, s(0)),
                    # Add 128 to msb of BOTH rs1 and imm
                    at(h.rs1[2], s(-2)),
                    loop(h.rs1[2], at(h.rs1[2], c(-2)) + at([h.rs1[3], h.rs2[3]], c(1))),
                    at([h.rs1[3], h.rs2[3]], c(1)),
                    move(h.xd, h.rd[0]),
                    move(h.x1, h.rd[1]),
                    move(h.x2, h.x1), move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_SLTU_1.f)),
                    g(-h.F + md.f - 2),
                )],
                [3, join(
                    # slt
                    at(h.F, s(0)),
                    move(h.xd, h.rd[0]),
                    move(h.x1, h.rd[1]),
                    move(h.x2, h.x1), move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_SLTU_1.f)),
                    g(-h.F + md.f - 2),
                )],
                [4, join(
                    # xor
                    at(h.F, s(0)),
                    move(h.xd, h.rd[0]),
                    move(h.x1, h.rd[1]),
                    move(h.x2, h.x1), move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_XOR_1.f)),
                    g(-h.F + md.f - 2),
                )],
                join(
                    # srl
                    at(h.F, s(0)),
                    move(h.xd, h.rd[0]),
                    move(h.x1, h.rd[1]),
                    move(h.x2, h.x1), move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_SRL_1.f)),
                    g(-h.F + md.f - 2),
                ),
                [6, join(
                    # or
                    at(h.F, s(0)),
                    move(h.xd, h.rd[0]),
                    move(h.x1, h.rd[1]),
                    move(h.x2, h.x1), move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_OR_1.f)),
                    g(-h.F + md.f - 2),
                )],
                [7, join(
                    # and
                    at(h.F, s(0)),
                    move(h.xd, h.rd[0]),
                    move(h.x1, h.rd[1]),
                    move(h.x2, h.x1), move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_AND_1.f)),
                    g(-h.F + md.f - 2),
                )],
                [8, join(
                    # sub
                    # load x2 into rs1
                    # invert and add 1
                    # load x1 into rs2
                    # call add
                    # store rs2 into xd
                    at(h.F, s(0)),
                    move(h.xd, h.rd[0]),
                    move(h.x1, h.rd[1]),
                    move(h.x2, h.x1), move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_SUB_1.f)),
                    g(-h.F + md.f - 2),
                )],
                [13, join(
                    # sra
                    at(h.F, s(0)),
                    move(h.xd, h.rd[0]),
                    move(h.x1, h.rd[1]),
                    move(h.x2, h.x1), move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(h.rd[3], s(1)),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_SRL_1.f)),
                    g(-h.F + md.f - 2),
                )],
            ),
        )],

        # ''',

        [5, join(  # case 5:
            # B type
            # beq (0), bne (1), blt (4), bge (5), bltu (6), bgeu (7)
            at(h.F, s(0)),
            # flags(rd[0:3]): negate, equal, signed
            move(h.funct_flag, h.F),
            switch(on=h.F)(  # TODO: switch to [-[-[- trick
                join(
                    at(h.F, s(ERROR.f)),
                    at(h.N, s(E_INVALID_FUNCT3_BRANCH)),
                    g(-2),
                ),
                join(  # beq
                    at(h.F, s(0)),
                    g(-2),
                ),
                join(  # bne
                    at(h.F, s(0)),
                    at(h.rd[0], s(1)),
                    g(-2),
                ),
                [4, join(  # blt
                    at(h.F, s(0)),
                    at(h.rd[1], s(1)),
                    at(h.rd[2], s(1)),
                    g(-2),
                )],
                join(  # bge
                    at(h.F, s(0)),
                    at(h.rd[0], s(1)),
                    at(h.rd[1], s(1)),
                    at(h.rd[2], s(1)),
                    g(-2),
                ),
                join(  # bltu
                    at(h.F, s(0)),
                    at(h.rd[1], s(1)),
                    g(-2),
                ),
                join(  # bltu
                    at(h.F, s(0)),
                    at(h.rd[0], s(1)),
                    at(h.rd[1], s(1)),
                    g(-2),
                ),
            ),
            g(+2),
            switch(on=h.F)(
                join(
                    g(-2),
                ),
                join(
                    zipmove(h.imm, h.rs2),
                    move(h.x2, h.rd[3]),
                    move(h.x1, 4*[h.mem.i[0], md.i[0]]),
                    at(h.F, s(0)),
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1)),
                    at(h.mem_next, s(_BRANCH_1.f)),
                    g(-h.F + md.f - 2),
                ),
            ),
            g(+2),
            g(-2),
        )],

        # *0*r'''

        [6, join(  # case 6:
            # I type
            # ecall (0), ebreak (1)
            # in either case, exit the interpreter loop.
            # we assume that surrounding brainfuck code will examine the state
            # of the interpreter and optionally resume the interpreter loop
            ON_SYSTEM(),
            g(-2),
        )],

        # ''',

        [7, join(  # case 7:
            # U type
            # auipc
            zipmove(h.pc, h.rd), zipmove(h.rd, zip(h.pc, h.rs2)),  # copy pc.. to rs2. using rd..
            zipmove(h.imm, h.rs1),  # imm. => rs1.
            move(h.xd, h.rd[0]),
            # call ADD with some next function
            at(h.F, s(ADD.f)),
            at(h.N, s(_AUIPC_1.f)),
            g(-2),
        )],

        [8, join(  # case 8:
            # U type
            # lui
            at(h.F, s(0)),
            zipmove(h.imm, md.rwp.data),
            at(md.rwp._t, s(1)),
            move(h.xd, 4*[h.mem.i[0], md.i[0]]),
            at(md.f, s(MEMMOVRIGHT.f)),
            at(md.n, s(1)),
            at(h.mem_next, s(INCPC.f)),
            g(-h.F + md.f - 2),
        )],

        [9, join(  # case 9:
            # I type
            # jalr

            # Pseudocode:
            # calculate pc + 4
            # store pc + 4
            # fetch rs1
            # calculate rs1 + imm
            # update pc and FETCH

            zipmove(h.pc, h.rs2),
            at(h.rs1[0], s(4)),
            at(h.F, s(ADD.f)),
            at(h.N, s(_JALR_1.f)),
            g(-2),
        )],

        [10, join(  # case 10:
            # J type
            # jal
            # call FETCH since we changed the PC

            # Pseudocode:
            # calculate pc + 4
            # store pc + 4
            # calculate pc + imm
            # update pc and FETCH

            zipmove(h.pc, h.rs1),
            zipmove(h.rs1, zip(h.pc, h.rs2)),
            at(h.rs1[0], s(4)),
            at(h.F, s(ADD.f)),
            at(h.N, s(_JAL_1.f)),
            g(-2),
        )],

        [11, join(  # case 11:
            # U type
            # fence
            # x1 xd imm
            # let wrapping code do something (maybe flush i/o idk)
            ON_FENCE(),
            g(-2),
        )],
    ) + g(+2)


@mark_function
def _LOAD_1():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        zipmove(md.rwp.data, h.rs2),  # xyzw => rs2.
        at(h.F, s(ADD.f)),  # F = ADD
        at(h.N, s(_LOAD_2.f)),  # N = _LOAD_2
    )

@mark_function
def _LOAD_2():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        at(h.F, s(0)),
        zipmove(h.rs2, md.i), # rs2. => IJKL
        # ijkl[0] = 128
        at(h.mem.i[1], s(-2)),
        loop(h.mem.i[1], at(h.mem.i[1], c(-2)) + at(h.mem.i[0], c(1))),
        at(h.mem.i[0], c(1)),
        at(md.f, s(MEMMOVRIGHT.f)),  # f = MEMMOVRIGHT
        at(md.n, s(0)),  # n = 0
        at(h.mem_next, s(_LOAD_3.f)),  # M = _LOAD_3
        g(-h.F + md.f),  # land __fn
    )

@mark_function
def _LOAD_3():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        at(h.F, s(0)), move(h.rd[0], h.F),  # rd..[0] => F
        at(h.N, s(0)), move(h.rd[1], h.N),  # rd..[1] => N
        switch(on=h.F)(  # switch F:
            join(  # default:
                at(h.F, s(ERROR.f)),  # F = ERROR
                at(h.N, s(E_INVALID_FUNCT3_LOAD)),
                g(-2) , # land __FN
            ),

            join(  # case 0:  # load byte (sign extend)
                at(md.rwp.data[1], s(0)),  # xyzw[1] = 0
                at(md.rwp.data[2], s(0)),  # xyzw[2] = 0
                at(md.rwp.data[3], s(0)),  # xyzw[3] = 0
                move(md.rwp.data[0], [md.i[1], md.i[3]]), move(md.i[3], md.rwp.data[0]),  # xyzw[0] => IJKL[1] + IJKL[3]; IJKL[3] => xyzw[0]
                at(md.i[0], c(-2)),  # IJKL[0] -= 2
                loop(md.i[0], join(  # while IJKL[0]:
                    at(md.i[0], c(-2)),  # IJKL[0] -= 2
                    at(md.i[2], c(1)),  # IJKL[2] += 1
                    loop(md.i[1], join(  # if IJKL[1]:
                        at(md.i[1], c(-1)),  # IJKL[1] -= 1
                        at(md.i[2], c(-1)),  # IJKL[2] -= 1
                        g(-md.i[1] + md.i[2]),  # land IJKL[2]
                    )),
                    loop(md.i[2], join(  # if IJKL[2]:
                        at(md.i[0], s(0)),  # IJKL[0] = 0
                        g(-md.i[2] + md.i[3]),  # land IJKL[3]
                    )),
                    g(-md.i[3] + md.i[2]),  # at IJKL[3]
                )),
                at(md.i[2], s(0)),
                ifnonzero(md.i[1], join(  # if IJKL[1]:
                    # IJKL[1] = 0
                    at(md.rwp.data[1], s(-1)),  # xyzw[1] = -1
                    at(md.rwp.data[2], s(-1)),  # xyzw[2] = -1
                    at(md.rwp.data[3], s(-1)),  # xyzw[3] = -1
                )),
                at(h.F, s(0)),  # F = 0
                g(-2),  # land __FN
            ),

            join(  # case 1:  # load half (sign extend)
                at(md.rwp.data[2], s(0)),  # xyzw[2] = 0
                at(md.rwp.data[3], s(0)),  # xyzw[3] = 0
                move(md.rwp.data[1], [md.i[1], md.i[3]]), move(md.i[3], md.rwp.data[1]),  # xyzw[1] => IJKL[1] + IJKL[3]; IJKL[3] => xyzw[1]
                at(md.i[0], c(-2)),  # IJKL[0] -= 2
                loop(md.i[0], join(  # while IJKL[0]:
                    at(md.i[0], c(-2)),  # IJKL[0] -= 2
                    at(md.i[2], c(1)),  # IJKL[2] += 1
                    loop(md.i[1], join(  # if IJKL[1]:
                        at(md.i[1], c(-1)),  # IJKL[1] -= 1
                        at(md.i[2], c(-1)),  # IJKL[2] -= 1
                        g(-md.i[1] + md.i[2]),  # land IJKL[2]
                    )),
                    loop(md.i[2], join(  # if IJKL[2]:
                        at(md.i[0], s(0)),  # IJKL[0] = 0
                        g(-md.i[2] + md.i[3]),  # land IJKL[3]
                    )),
                    g(-md.i[3] + md.i[2]),  # at IJKL[3]
                )),
                at(md.i[2], s(0)),
                ifnonzero(md.i[1], join(  # if IJKL[1]:
                    # IJKL[1] = 0
                    at(md.rwp.data[2], s(-1)),  # xyzw[2] = -1
                    at(md.rwp.data[3], s(-1)),  # xyzw[3] = -1
                )),
                at(h.F, s(0)),  # F = 0
                g(-2),  # land __FN
            ),

            join(  # case 2:  # load word
                at(h.F, s(0)),  # F = 0
                g(-2),  # land __FN
            ),

            [4, join(  # case 4:  # load byte
                at(md.rwp.data[1], s(0)),  # xyzw[1] = 0
                at(md.rwp.data[2], s(0)),  # xyzw[2] = 0
                at(md.rwp.data[3], s(0)),  # xyzw[3] = 0
                at(h.F, s(0)),  # F = 0
                g(-2),  # land __FN
            )],

            join(  # case 5:  # load half
                at(md.rwp.data[2], s(0)),  # xyzw[2] = 0
                at(md.rwp.data[3], s(0)),  # xyzw[3] = 0
                at(h.F, s(0)),  # F = 0
                g(-2),  # land __FN
            ),
        ),
        g(+2),

        switch(on=h.F)(  # switch F:
            join(  # default:
                at(h.F, s(ERROR.f)),  # F = ERROR
                g(-2), # land __FN
            ),
            join(  # case 0:
                at(h.F, s(0)),
                move(h.N, 4*[h.mem.i[0], md.i[0]]),  # while N: N -= 1; ijkl[0] += 4 IJKL[0] += 4
                at(md.rwp._t, s(1)),  # xyzw_[4] = 1  # write all
                at(md.f, s(MEMMOVRIGHT.f)),  # f = MEMMOVRIGHT
                at(md.n, s(1)),  # n = 1
                at(h.mem_next, s(INCPC.f)),  # M = INCPC
                g(-h.F + md.f - 2),  # land __fn
            ),
        ),
        g(+2),
    )

@mark_function
def _STORE_1():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        zipmove(md.rwp.data, h.rs2),
        at(h.F, s(ADD.f)),
        at(h.N, s(_STORE_2.f)),
    )

@mark_function
def _STORE_2():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        at(h.F, s(0)),
        move(h.rd[1], 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(_STORE_3.f)),
        g(-h.F + md.f),
    )

@mark_function
def _STORE_3():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        at(h.F, s(0)),
        at(h.rd[1], s(2)),
        loopdown(h.rd[0], at(h.rd[1], c(-1))),
        at(md.rwp._t, s(1)),
        ifnonzero(h.rd[1], join(
            at(md.rwp._t, s(0)),
            at(md.rwp.data[3], s(0)),
            at(md.rwp.data[2], s(1)),
            at(h.rd[1], c(-1)),
            ifnonzero(h.rd[1], join(
                at(md.rwp.data[2], s(0)),
                at(md.rwp.data[1], s(1)),
                at(h.rd[1], c(-1)),
                ifnonzero(h.rd[1], join(
                    at(h.F, s(1)),
                )),
            )),
        )),
        zipmove(h.rs2, md.i),
        at(h.mem.i[1], s(-2)),
        loop(h.mem.i[1], at(h.mem.i[1], c(-2)) + at(h.mem.i[0], c(1))),
        at(h.mem.i[0], c(1)),
        switch(on=h.F)(
            join(
                at(h.F, s(ERROR.f)),
                at(h.N, s(E_INVALID_FUNCT3_STORE)),
                g(-2),
            ),
            join(
                at(md.f, s(MEMMOVRIGHT.f)),
                at(md.n, s(0)),
                at(h.mem_next, s(INCPC.f)),
                g(-h.F + md.f - 2)
            ),
        ),
        g(+2),
    )

@mark_function
def _ADDI_1():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        zipmove(md.rwp.data, h.rs2),
        at(h.F, s(ADD.f)),
        at(h.N, s(_ADDI_2.f)),
    )

@mark_function
def _ADDI_2():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        at(h.F, s(0)),
        zipmove(h.rs2, md.rwp.data),
        at(md.rwp._t, s(1)),
        move(h.rd[0], 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(INCPC.f)),
        g(-h.F + md.f),
    )

@mark_function
def _SLLI_1():
    h = DECODE_FORMAT(); md = h.mem.data
    BBB = [*h.mem.i, h.mem._a, *md.land, md.f, md.n]
    return join(
        at(h.F, s(0)),
        loopdown(h.rd[2], join(
            at(md.rwp.data[3], s(0)),
            move(md.rwp.data[2], md.rwp.data[3]),
            move(md.rwp.data[1], md.rwp.data[2]),
            move(md.rwp.data[0], md.rwp.data[1]),
        )),
        ifnonzero(h.rd[1], join(
#                                               46   51      59
#                                          _____ijkl___fnIJKLxyzw_
#                                         40  44
#                         24              M   S12dimm.
#                         rs1.rs2.rd..pc..__FN
# ___-aA1_bB1_cC1_dD1_____
# xy-_aAbBcCdDeEfFgGhH__
            move(h.rd[1], md.i),  # not a zipmove

            loop(md.i[0], join(
                unpack_bits(md.rwp.data[0], BBB[::-1]),  # yes unpack in reverse
                at(BBB[-1], s(-1)),
                loopdown(md.i[0], join(
                    move(h.mem.decr_flag, 2*[BBB[0]]),
                    loopuntil(-1, BBB[0], join(
                        move(BBB[0], h.mem.decr_flag),
                        g(-BBB[0] + BBB[1]),
                    )),
                    g(-BBB[-1] + BBB[0]),
                )),
                # h.mem.decr_flag has next byte's lower bits
                loopuntil(-1, BBB[1], join(
                    move(BBB[0], 2*[BBB[1]]),
                    g(-BBB[1] + BBB[2]),
                )),
                g(-BBB[-1] + BBB[1]),
                move(BBB[-2], h.mem.decr_flag-1),
                # h.mem.decr_flag-1 has this byte's higher bits
                at(BBB[-1], s(0)),  # change to c(1)
                g(-md.i[0] + md.i[1]),
            )),
            g(-md.rwp.data[0] + md.i[0]),
            # M_____ijkl___fnIJKLxyzw_
                # XY--------.1111xyzw_
                # XYZWV clear V

            at([h.mem_next, *h.mem._t[:3]], s(1)),
            loop(h.mem_next, join(
                at(h.mem_next, s(0)),
                move(h.mem.decr_flag-1, md.rwp.data[0]),
                g(-h.mem_next + h.mem._t[0]),
            )),
            g(-h.mem._t[3] + h.mem_next),
            at(BBB[2], s(0)),
        )),
        at(md.rwp._t, s(1)),
        move(h.rd[0], 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(INCPC.f)),
        g(-h.F + md.f),
    )


@mark_function
def _XORI_1():
    return _BITOPI_1("xor")
@mark_function
def _ORI_1():
    return _BITOPI_1("or")
@mark_function
def _ANDI_1():
    return _BITOPI_1("and")

def _BITOPI_1(kind):
    h = DECODE_FORMAT(); md = h.mem.data
                                         # _____ijkl___fnIJKLxyzw_
                                        # 40  44
                        # 24              M   S12dimm.
                        # rs1.rs2.rd..pc..__FN
# ___-aA1_bB1_cC1_dD1_____
# xy-_aAbBcCdDeEfFgGhH__
    return join(
        at(h.F, s(0)),
        at([h.bits.bits1[9], h.bits.bits2[9], h.bits.bits2[9]+1, h.bits.bits2[9]+2], s(1)),
        loop(h.bits.bits1[9], join(
            at(h.bits.bits1[9], s(0)),
            move(md.rwp.data[0], h.bits.byte2),
            move(h.rs1[0], h.bits.byte1),
            getattr(h.bits, f'bitwise_{kind}')(),
            move(h.bits.byte1, md.rwp.data[0]),
            g(-h.bits.bits1[9] + h.bits.bits2[9]),
        )),
        g(-(h.bits.bits2[9]+3) + h.bits.bits1[9]),
        at(md.rwp._t, s(1)),
        move(h.rd[0], 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(INCPC.f)),
        g(-h.F + md.f),
    )

@mark_function
def _SLTIU_1():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        zipmove(md.rwp.data, h.rs1),
        at(h.F, s(LESSTHAN.f)),
        at(h.N, s(_SLTIU_2.f)),
    )

@mark_function
def _SLTIU_2():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        at(h.F, s(0)),
        move(h.rs2[0], md.rwp.data[0]),
        at(md.rwp._t, s(1)),
        move(h.rd[0], 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(INCPC.f)),
        g(-h.F + md.f),
    )

@mark_function
def _SRLI_1():
    h = DECODE_FORMAT(); md = h.mem.data
    # rd[0] has xd
    # rd[1] has bitshift
    # rd[2] has byteshift
    # rd[3] has keepsign
    BBB = [*h.mem.i, h.mem._a, *md.land, md.f, md.n]
    return join(
        at(h.F, s(0)),
        ifnonzero(h.rd[3], join(
            at(h.rd[3], s(0)),
            move(md.rwp.data[3], h.rd[3]),
            move(h.rd[3], [md.rwp.data[3], md.rwp._t]),
            unpack_bits(md.rwp._t, BBB[::-1]),  # yes unpack in reverse
            at(BBB[1:-1], s(0)),
        )),
        move(BBB[0], h.rd[3]),
        move(h.rd[1], BBB[0]),
        ifnonzero(BBB[0], join(
            at(h.rd[1], s(8)),
            loopdown(BBB[0], at(h.rd[1], c(-1))),
        )),
        loopdown(h.rd[2], join(
            at(md.rwp.data[0], s(0)),
            move(md.rwp.data[1], md.rwp.data[0]),
            move(md.rwp.data[2], md.rwp.data[1]),
            move(md.rwp.data[3], md.rwp.data[2]),
            move(h.rd[3], BBB[0]),
            ifnonzero(BBB[0], at(h.rd[3], c(1)) + at(md.rwp.data[3], c(-1))),
        )),
        ifnonzero(h.rd[1], join(

            move(h.rd[1], [h.rd[2], *md.i]),
            # got 8-bit
            loop(md.i[0], join(
                unpack_bits(md.rwp.data[0], BBB[::-1]),  # yes unpack in reverse
                at(BBB[0]-2, s(0)),
                move(BBB[0], BBB[0]-2),
                move(BBB[0]-2, [BBB[0], BBB[0]-1]),

                at(BBB[-1], s(-1)),
                loopdown(md.i[0], join(
                    move(BBB[0]-3, 2*[BBB[0]]),
                    move(BBB[0], BBB[0]-3),
                    loopuntil(-1, BBB[1], join(
                        move(BBB[1], BBB[0]),
                        g(-BBB[1] + BBB[2]),
                    )),
                    g(-BBB[-1] + BBB[1]),
                )),
                loopuntil(-1, BBB[1], join(
                    move(BBB[0], 2*[BBB[1]]),
                    g(-BBB[1] + BBB[2]),
                )),
                g(-BBB[-1] + BBB[1]),
                # BBB[0]-3 upper
                # BBB[-2] lower
                move(BBB[-2], BBB[0]-4),
                at(BBB[-1], c(1)),
                g(-md.i[0] + md.i[1]),
            )),
            g(-md.rwp.data[0] + md.i[0]),
            # M_____ijkl___fnIJKLxyzw_
               # X__--------.1111xyzw_
                # XYZWV clear X

            at(BBB[0]-4, s(0)),
            move(BBB[2], BBB[6]),
            at(BBB[1:5], s(1)),
            loop(BBB[1], join(
                at(BBB[1], s(0)),
                move(BBB[0]-3, md.rwp.data[0]),
                g(-BBB[1] + BBB[2]),
            )),
            g(-BBB[5] + BBB[1]),

            ifnonzero(h.rd[3], ifnonzero(BBB[6], join(
                loopdown(h.rd[2], join(
                    move(h.rd[3], h.rd[1]),
                    move(h.rd[1], 2*[h.rd[3]]),
                )),
                loopdown(h.rd[3], at(md.rwp.data[3], c(-1))),
            ))),
            at([BBB[6], h.rd[2]], s(0)),
        )),
        at(md.rwp._t, s(1)),
        move(h.rd[0], 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(INCPC.f)),
        g(-h.F + md.f),
    )

@mark_function
def _AUIPC_1():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        at(h.F, s(0)),
        zipmove(h.rs2, md.rwp.data),
        at(md.rwp._t, s(1)),
        move(h.rd[0], 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(INCPC.f)),
        g(-h.F + md.f),
    )

@mark_function
def _JALR_1():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        move(h.x1, h.rd[1]),
        at(h.F, s(0)),
        zipmove(h.rs2, md.rwp.data),
        zipmove(h.imm, h.rs1),
        at(md.rwp._t, s(1)),
        move(h.xd, 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(_JALR_2.f)),
        g(-h.F + md.f),
    )

@mark_function
def _JALR_2():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        at(h.F, s(0)),
        move(h.rd[1], 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(_JALR_3.f)),
        g(-h.F + md.f),
    )

@mark_function
def _JALR_3():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        zipmove(md.rwp.data, h.rs2),
        at(h.F, s(ADD.f)),
        at(h.N, s(_JALR_4.f)),
    )

@mark_function
def _JALR_4():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        zipmove(h.rs2, h.pc),
        at(h.F, s(FETCH.f)),
    )

@mark_function
def _JAL_1():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        at(h.F, s(0)),
        zipmove(h.rs2, md.rwp.data),
        zipmove(h.imm, h.rs1),
        at(md.rwp._t, s(1)),
        move(h.xd, 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(_JAL_2.f)),
        g(-h.F + md.f),
    )

@mark_function
def _JAL_2():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        zipmove(h.pc, h.rs2),
        at(h.F, s(ADD.f)),
        at(h.N, s(_JALR_4.f)),
    )

@mark_function
def _BRANCH_1():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        at(h.F, s(0)),
        zipmove(md.rwp.data, h.rs1),
        move(h.rd[3], 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(_BRANCH_2.f)),
        g(-h.F + md.f),
    )

@mark_function
def _BRANCH_2():
    h = DECODE_FORMAT(); md = h.mem.data
    # flags(rd[0:3]): negate, equal, signed
    return join(
        zipmove(h.rs2, h.imm),
        zipmove(md.rwp.data, h.rs2),
        # if signed add 128 to msb of both rs1 and rs2
        # https://esolangs.org/wiki/Brainfuck_constants#128
        ifnonzero(h.rd[2], join(
            at(h.rd[2], s(-2)),
            loop(h.rd[2], at(h.rd[2], c(-2)) + at([h.rs1[3], h.rs2[3]], c(1))),
            at([h.rs1[3], h.rs2[3]], c(1)),
        )),
        at(h.F, s(0)),
        # switch on comparison type
        move(h.rd[1], h.F),
        switch(on=h.F)(
            join(
                # lessthan
                at(h.F, s(LESSTHAN.f)),
                at(h.N, s(_BRANCH_3.f)),
                g(-2),
            ),
            join(
                # equal
                join(loopdown(a, at(b, c(-1))) for a, b in zip(h.rs1, h.rs2)),
                ifnonzero(h.rs2[0], at(h.rs2[1], s(1))),
                at(h.rs2[0], c(1)),
                ifnonzero(h.rs2[1], at(h.rs2[2], s(1))),
                ifnonzero(h.rs2[2], at(h.rs2[3], s(1))),
                ifnonzero(h.rs2[3], at(h.rs2[0], c(-1))),
                at(h.F, s(_BRANCH_3.f)),  # call aftercompare function
                g(-2),
            ),
        ),
        g(+2),
    )

@mark_function
def _BRANCH_3():
    # [aftercompare function]
    h = DECODE_FORMAT(); md = h.mem.data
    # flags(rd[0:3]): negate, equal, signed
    return join(
        at(h.F, s(0)),
        move(h.rs2[0], h.F),
        ifnonzero(h.rd[0], join(
            move(h.F, h.rs2[0]),
            at(h.F, c(1)),
            loopdown(h.rs2[0], at(h.F, c(-1))),
        )),
        switch(on=h.F)(
            join(
                zipmove(h.imm, h.rs1),
                at(h.F, s(_JAL_2.f)),
                g(-2),
            ),
            join(
                at(h.imm, s(0)),
                at(h.F, s(INCPC.f)),
                g(-2),
            ),
        ),
        g(+2),
    )

@mark_function
def _ADD_1():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        zipmove(md.rwp.data, h.rs1),
        at(h.F, s(0)),
        move(h.rd[1], 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(_ADDI_1.f)),  # rest of the logic is the same
        g(-h.F + md.f),
    )

@mark_function
def _SUB_1():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        # Invert all bits. Note that ~x == -(x+1)
        # https://esolangs.org/wiki/Brainfuck_algorithms#x%C2%B4_=_not_x_(bitwise)
        join(at(md.rwp.data[i], c(+1)) + loopdown(md.rwp.data[i], at(h.rs2[i], c(-1))) for i in range(len(md.rwp.data))),
        # add 1
        at(h.rs1[0], s(1)),
        at(h.F, s(ADD.f)),
        at(h.N, s(_SUB_2.f)),
    )

@mark_function
def _SUB_2():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        at(h.F, s(0)),
        zipmove(h.rs2, h.rs1),
        move(h.rd[1], 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(_ADDI_1.f)),  # rest of the logic is the same
        g(-h.F + md.f),
    )

@mark_function
def _XOR_1():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        zipmove(md.rwp.data, h.rs1),
        at(h.F, s(0)),
        move(h.rd[1], 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(_XORI_1.f)),  # rest of the logic is the same
        g(-h.F + md.f),
    )

@mark_function
def _OR_1():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        zipmove(md.rwp.data, h.rs1),
        at(h.F, s(0)),
        move(h.rd[1], 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(_ORI_1.f)),  # rest of the logic is the same
        g(-h.F + md.f),
    )

@mark_function
def _AND_1():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        zipmove(md.rwp.data, h.rs1),
        at(h.F, s(0)),
        move(h.rd[1], 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(_ANDI_1.f)),  # rest of the logic is the same
        g(-h.F + md.f),
    )

@mark_function
def _SLTU_1():
    h = DECODE_FORMAT(); md = h.mem.data
    return join(
        zipmove(md.rwp.data, h.rs2),
        at(h.F, s(0)),
        move(h.rd[1], 4*[h.mem.i[0], md.i[0]]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(_SLTIU_1.f)),  # rest of the logic is the same
        g(-h.F + md.f),
    )

@mark_function
def _SLL_1():
    h = DECODE_FORMAT(); md = h.mem.data
    B4 = [md.n, *md.i, *md.rwp.data]
    return join(
        # data has rs2
        at(h.F, s(0)),
        at(md.rwp.data[1:], s(0)),
        # find shift amount (unpack into nIJKLxyzw and get lower 5 bits)
        move(md.rwp.data[0], md.rwp._t),
        unpack_bits(md.rwp._t, B4[::-1]),
        at(B4[:3], s(0)),
        move(B4[3], 2*[B4[4]]),
        move(B4[5], 2*[B4[6]]),
        move(B4[6], 2*[B4[7]]),
        # B4[4] has byteshift
        # B4[7] has bitshift
        move(B4[4], h.rd[2]),
        move(h.rd[1], 4*[h.mem.i[0], md.i[0]]),
        move(B4[7], h.rd[1]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(_SLLI_1.f)),  # rest of the logic is the same
        g(-h.F + md.f),
    )

@mark_function
def _SRL_1():
    h = DECODE_FORMAT(); md = h.mem.data
    B4 = [md.n, *md.i, *md.rwp.data]
    return join(
        # data has rs2
        at(h.F, s(0)),
        at(md.rwp.data[1:], s(0)),
        # find shift amount (unpack into nIJKLxyzw and get lower 5 bits)
        move(md.rwp.data[0], md.rwp._t),
        unpack_bits(md.rwp._t, B4[::-1]),
        at(B4[:3], s(0)),
        move(B4[3], 2*[B4[4]]),
        move(B4[5], 2*[B4[6]]),
        move(B4[6], 2*[B4[7]]),
        # B4[4] has byteshift
        # B4[7] has bitshift
        move(B4[4], h.rd[2]),
        move(h.rd[1], 4*[h.mem.i[0], md.i[0]]),
        move(B4[7], h.rd[1]),
        at(md.f, s(MEMMOVRIGHT.f)),
        at(md.n, s(1)),
        at(h.mem_next, s(_SRLI_1.f)),  # rest of the logic is the same
        g(-h.F + md.f),
    )


for function in functions:
    if hasattr(function, "name"):
        _DEBUG_ and print(function.name, function.f)
    else:
        _DEBUG_ and print(function.__name__, function.f)

if __name__ == "__main__":
    if "--output-mainloop" in sys.argv:
        code = MAINLOOP()
        import compressbf; code = compressbf.compress(","+code)[1:]
        class KeepCharsTable:
            def __init__(self, keepchars):
                self.keepchars = keepchars
            def __getitem__(self, ordinal):
                return ordinal if chr(ordinal) in self.keepchars else None
        code = code.translate(KeepCharsTable("+-<>,.[]"))
        print("A rudimentary RISCV32I interpreter by GeeTransit".center(80).rstrip())
        print("Place RISCV instructions starting at 192 cells to the right".center(80).rstrip())
        for i in range(len(code))[::80]:
            print(code[i:i+80])

    # TODO: Add wrapping environment
    elif "--output-simple-iobreak" in sys.argv:
        code = MAINLOOP().replace("#", "")
        import compressbf; code = compressbf.compress(","+code)[1:]
        # format: we take up 64 bytes at the start to align nicely with the rest
        # of the head. however, we only use the last 4 bytes as state (landing)
        L4 = shift(range(4), 60)
        LF = L4[2]; LN = L4[3]
        LZ = L4[0] + len(L4)
        hhead = shift(DECODE_FORMAT(), LZ)
        iobreak_code = join(
            at(LN, c(1)),
            loopuntil(ERROR.f, LF, join(
                switch(on=LF)(
                    join(
                        at(LF, s(ERROR.f)),
                        g(-2),
                    ),
                    # 0: run mainloop until error
                    [0, join(
                        at(LF, s(0)),
                        move(LN, LF),
                        at(LZ, code),
                        at(hhead.F, s(0)),
                        move(hhead.N, LN),
                        g(-2),
                    )],
                    # 1: check error type
                    [1, join(
                        at(LF, s(0)),
                        move(LN, LF),
                        switch(on=LF)(
                            join(
                                # unknown??
                                at(LF, s(ERROR.f)),
                                g(-2),
                            ),
                            # 7: ecall, ebreak
                            [E_SYSTEM, join(
                                # switch on rs2
                                at(LF, s(0)),
                                ifnonzero(hhead.rs2[3], at(hhead.rs2[2], s(1))),
                                ifnonzero(hhead.rs2[2], at(hhead.rs2[1], s(1))),
                                ifnonzero(hhead.rs2[1], at(hhead.rs2[0], s(2))),
                                move(hhead.rs2[0], LF),
                                switch(on=LF)(
                                    join(
                                        # unknown :(
                                        at(LF, s(ERROR.f)),
                                        g(-2),
                                    ),
                                    # 0: ecall
                                    [0, join(
                                        # switch on the a7 (x17) register
                                        at(LF, s(0)),
                                        move(hhead.end + 17*4, LF),
                                        switch(on=LF)(
                                            join(
                                                # unknown :(
                                                at(LF, s(ERROR.f)),
                                                g(-2),
                                            ),
                                            # 0: input into a0 (x10)
                                            [0, join(
                                                at(hhead.end + 10*4, ","),
                                                at(hhead.end + 10*4 + 1, s(0)),
                                                at(hhead.end + 10*4 + 2, s(0)),
                                                at(hhead.end + 10*4 + 3, s(0)),
                                                at(hhead.F, s(INCPC.f)),
                                                at(LF, s(0)),
                                                at(LN, s(1)),
                                                g(-2),
                                            )],
                                            # 1: output from a0 (x10)
                                            [1, join(
                                                at(hhead.end + 10*4, "."),
                                                at(hhead.F, s(INCPC.f)),
                                                at(LF, s(0)),
                                                at(LN, s(1)),
                                                g(-2),
                                            )],
                                            # 2: exit
                                            [1, join(
                                                at(LF, s(ERROR.f)),
                                                at(LN, s(0)),
                                                g(-2),
                                            )],
                                        ),
                                    )],
                                    # 1: ebreak
                                    [1, join(
                                        "#",
                                        at(LF, s(0)),
                                        at(hhead.F, s(INCPC.f)),
                                        at(LN, s(1)),
                                        g(-2),
                                    )],
                                ),
                            )],
                            # 8: fence (noop)
                            [E_FENCE, join(
                                at(hhead.F, s(INCPC.f)),
                                at(LF, s(0)),
                                at(LN, s(1)),
                            )],
                        ),
                    )],
                ),
                g(+2),
            )),
        )
        import compressbf; code = compressbf.compress(","+iobreak_code)[1:]
        class KeepCharsTable:
            def __init__(self, keepchars): self.keepchars = keepchars
            def __getitem__(self, ordinal): return ordinal if chr(ordinal) in self.keepchars else None
        code = code.translate(KeepCharsTable("+-<>,.[]"))
        print("A rudimentary RISCV32I interpreter by GeeTransit".center(80).rstrip())
        print("Only 0=input 1=output 2=exit syscalls supported".center(80).rstrip())
        print("Place RISCV instructions starting at 256 cells to the right".center(80).rstrip())
        for i in range(len(code))[::80]:
            print(code[i:i+80])

    elif "--convert-hex-to-bf" in sys.argv or "--convert-bin-to-bf" in sys.argv:
        if "--offset" in sys.argv:
            i = sys.argv.index("--offset")
            sys.argv.pop(i)
            amount = int(sys.argv.pop(i))
        else:
            amount = 0
        import sys
        if "--convert-bin-to-bf" in sys.argv:
            inp = sys.stdin.buffer.read()
            code = at(amount, init(inp))
        else:
            inp = sys.stdin.read()
            code = at(amount, init([
                value
                for part in inp.split()
                if part.strip()
                if not part.strip().startswith("#")
                for value in int(part.strip(), 16).to_bytes(5, "little", signed=True)[:4]
            ]))
        import compressbf; code = compressbf.compress(","+code)[1:]
        class KeepCharsTable:
            def __init__(self, keepchars): self.keepchars = keepchars
            def __getitem__(self, ordinal): return ordinal if chr(ordinal) in self.keepchars else None
        code = code.translate(KeepCharsTable("+-<>,.[]"))
        for i in range(len(code))[::80]:
            print(code[i:i+80])

    elif "--output-fake-linux" in sys.argv:
        raise NotImplementedError
        # TODO: complete this
        # r'''
        original_functions = functions.copy()
        try:
            def _memmovright_call(callback, register=None, address=None, write=None, read=None):
                h = DECODE_FORMAT(); md = h.mem.data
                return join(
                    move(register, 4*[h.mem.i[0], md.i[0]])
                    if register is not None
                    else join(
                        zipmove(address, md.i),
                        at(h.mem.i[1], s(-2)),
                        loop(h.mem.i[1], at(h.mem.i[1], c(-2)) + at(h.mem.i[0], c(1))),
                        at(h.mem.i[0], c(1)),
                    ),
                    join(
                        zipmove(write, md.rwp.data),
                        at(md.rwp._t, s(1)),
                    ) if write is not None else "",
                    join(
                        at(md.rwp.data, s(0)),
                        at(md.rwp._t, s(0)),
                    ) if read is not None else "",
                    at(md.f, s(MEMMOVRIGHT.f)),
                    at(md.n, s(1 if register is not None else 0)),
                    at(h.mem_next, s(callback)),
                    g(-h.F + md.f),
                )
            def _call(function, n=None):
                h = DECODE_FORMAT(); md = h.mem.data
                return join(
                    at(h.F, s(function)),
                    at(h.N, n) if n is not None else "",
                )
            # def patch_function(func=None, *, name=None):
                # if func is None or isinstance(func, str):
                    # if isinstance(func, str): name = func
                    # return lambda func, *, name=name: patch_function(func, name=name)
                # for i, function in enumerate(functions):
                    # if getattr(function, "name", getattr(function, "__name__")):
                        # functions[i] = func
                        # func.f = function.f
                        # return
                # raise LookupError(f'cannot find function named {name}')
            old_on_fence = ON_FENCE
            def ON_MAINLOOP(): return ""
            def ON_FENCE(): h = DECODE_FORMAT(); return at(h.F, s(INCPC.f)) + at(h.N, s(0))
            def ON_SYSTEM():
                h = DECODE_FORMAT(); md = h.mem.data
                return join(
                    at(h.F, s(0)),
                    move(h.x2, h.F),
                    # collect other fields into F=2
                    ifnonzero(h.imm[0], at(h.xd, s(1))),
                    ifnonzero(h.xd, at(h.x1, s(1))),
                    ifnonzero(h.x1, at(h.funct_flag, s(1))),
                    ifnonzero(h.funct_flag, at(h.F, s(2))),
                    switch(on=h.F)(
                        default,
                        [0, join(  # ecall
                            at(h.F, s(0)),
                            # get x17
                            _memmovright_call(
                                register=h.x1,
                                callback=_ECALL_1.f,
                            ),
                            g(-2),
                        )],
                        [1, join(  # ebreak
                            at(h.F, "#"),
                            at(h.F, s(INCPC.f)),
                            g(-2),
                        )],
                    ),
                    g(+2),
                )
            @mark_function
            def _ECALL_1():
                h = DECODE_FORMAT(); md = h.mem.data
                return join(
                    # md.rwp.data has syscall number
                    # collect higher bytes into [0]=0
                    ifnonzero(md.rwp.data[3], at(md.rwp.data[2], s(1))),
                    ifnonzero(md.rwp.data[2], at(md.rwp.data[1], s(1))),
                    ifnonzero(md.rwp.data[1], at(md.rwp.data[0], s(0))),
                    move(md.rwp.data[0], h.F),
                    switch(on=h.F)(
                        join(
                            # invalid syscall error (set 10 to -1 for now)
                            at(h.imm[0], s(-1)),
                            at(h.x1, s(10)),
                            _memmovright_call(
                                register=h.x1,
                                callback=_ECALL_1.f,
                                write=h.imm,
                            ),
                            g(-2),
                        ),
                        [63, join(
                            # 63: read(10.fd, 11.*buf, 12.nbytes)
                            at(h.x1, s(12)),
                            _memmovright_call(
                                register=h.x1,
                                callback=_SYS_READ_1.f,
                                read=True,
                            ),
                            g(-2),
                        )],
                        # [64, join(
                            # 64: write(10.fd, 11.*buf, 12.nbytes)
                            # copy nbytes into rs2
                            # copy buf into rd
                            # while rs2
                                # rs2 -= 1
                                # fetch [rd]
                                # output md.rwp.data[0]
                                # rd += 1
                            # at(h.F, s(0)),
                        # )],
                        [93, join(
                            # 93: exit(10.code)
                            # ignore exit code
                            at(h.F, s(ERROR.f)),
                            at(h.N, s(6)),
                            g(-2),
                        )],
                    ),
                    g(+2),
                )
            @mark_function
            def _SYS_READ_1():
               h = DECODE_FORMAT(); md = h.mem.data; return join(
                    zipmove(md.rwp.data, h.rs2),
                    at(h.x1, s(12)),
                    _memmovright_call(
                        register=h.x1,
                        callback=_SYS_READ_2.f,
                        read=True,
                    ),
                    g(-2),
                )
            @mark_function
            def _SYS_READ_2():
                h = DECODE_FORMAT(); md = h.mem.data; return join(
                    zipmove(md.rwp.data, h.rsd),
                    _call(_SYS_READ_3).
                    g(-2),
                )
            @mark_function
            def _SYS_READ_3():
                # while rs2
                    # rs2 -= 1
                    # input md.rwp.data[0]
                    # write 1 byte
                    # rd += 1
                at(h.F, s()),
            code = MAINLOOP()
            import compressbf; code = compressbf.compress(","+code)[1:]
            class KeepCharsTable:
                def __init__(self, keepchars):
                    self.keepchars = keepchars
                def __getitem__(self, ordinal):
                    return ordinal if chr(ordinal) in self.keepchars else None
            code = code.translate(KeepCharsTable("+-<>,.[]"))
            print("A rudimentary RISCV32I interpreter by GeeTransit".center(80).rstrip())
            print("Only 63=read 64=write 93=exit syscalls supported".center(80).rstrip())
            print("Place RISCV instructions starting at 192 cells to the right".center(80).rstrip())
            for i in range(len(code))[::80]:
                print(code[i:i+80])
        finally:
            functions = original_functions
        # '''
        # format: we take up 64 bytes at the start to align nicely with the rest
        # of the head. however, we only use the last 4 bytes as state (landing)
        L = 62; LN = 63
        LZ = 64
        L4 = shift(range(4), 60)
        hhead = shift(DECODE_FORMAT(), LZ)
        r'''
        return join(
            at(LN, c(1)),
            loopuntil(-1, L, join(
                switch(on=L)(
                    join(
                        g(-2),
                    ),
                    # 0: run mainloop until error
                    [0, join(
                        at(L, s(0)),
                        move(LN, L),
                        at(LZ, code),
                        at(hhead.F, s(0)),
                        move(hhead.N, LN),
                        g(-2),
                    )],
                    # 1: check error type
                    [1, join(
                        at(L, s(0)),
                        move(LN, L),
                        switch(on=L)(
                            join(
                                # unknown??
                                at(L, s(-1)),
                                g(-2),
                            )
                            # 7: ecall, ebreak
                            [7, join(
                                # switch on rs2
                                at(L, s(0)),
                                move(LN, L),
                                switch(on=L)(
                                    join(
                                        # unknown :(
                                    ),
                                    # 0: ecall
                                    [0, join(
                                        # switch on the a7 (x17) register
                                        at(L, s(0)),
                                        move(x17, L),
                                        switch(on=L)(
                                            join(
                                                # unknown :(
                                            ),
                                            # 63: read(10.fd, 11.*buf, 12.nbytes)
                                            [63, join(
                                                # copy nbytes to state using state-4
                                                zipmove(shift(range(4), len(hhead) + 12*4), shift(L4, -4)),
                                                zipmove(shift(L4, -4), zip(range(4), shift(len(hhead) + 12*4), L4)),
                                                # set L -1, LN 5
                                                # call LESSTHAN rs2=1
                                                # move rs2[0] to L
                                                # switch L
                                                #   0: MEMMOVRIGHT fetch the memory
                                                #      go to lowest byte, output
                                                #      clear and go back
                                                # ahhhh
                                                # if any of the upper bytes are nonzero, make the lower one 2
                                                ifnonzero(L4[3], at(L4[2], s(2))),
                                                ifnonzero(L4[2], at(L4[1], s(2))),
                                                ifnonzero(L4[1], at(L4[0], s(2))),
                                                move(L4[0], L),
                                                switch(on=L)(
                                                    join(
                                                        # nonzero, call some
                                                at(L, s(-1)),
                                                move(len(hhead) + 10*4, LN),
                                                g(-2),
                                            )],
                                            # 64: write(10.fd, 11.*buf, 12.nbytes)
                                            [64, join(
                                                # we can't really do anything with the exit code lol
                                                at(L, s(-1)),
                                                move(len(hhead) + 10*4, LN),
                                                g(-2),
                                            )],
                                            # 93: exit(10.code)
                                            [93, join(
                                                # we can't really do anything with the exit code lol
                                                at(L, s(-1)),
                                                move(len(hhead) + 10*4, LN),
                                                g(-2),
                                            )],
                                        ),
                                    )],
                                    # 1: ebreak
                                    [1, join(
                                    )],
                                ),
                            )],
                            # 8: fence (noop)
                            [8, join(
                            )],
                        ),
                    )],
                ),
                g(+2),
            ),
        )
        '''


r'''
https://chipyard.readthedocs.io/en/stable/Software/Baremetal.html

4, 38, 42 opcodes unused (maybe repurpose for single byte i/o)
or maybe switch on the length and only do it for length 1 reads/writes

misa hardcoded to 010...I...
mvendorid 0
marchid 0
mimpid 0
mhartid 0
mstatus 0?
mstatush 0?

f.0011000 2.00010 1.00000 3.000 d.00000 1110011 mret
f.0001000 2.00101 1.00000 3.000 d.00000 1110011 wfi

>[>[>+>]]<[<]
_ab__
_a___
__b__

255 special char
128 select device

0 text
1 graphic
2 keyboard
3 os
4 filesystem

default device is 0
input

if char == 255:
    wait for next char
    if char == 255:
        input char
    elif char >= 128:
        select device char - 128
        error if invalid device?
'''
