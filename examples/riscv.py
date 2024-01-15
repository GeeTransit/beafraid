"""A simple RISC-V 32I emulator in Brainfuck"""

from beafraid import *
from .s4op import pack_class_offsets, shift, size, start, Struct, Offset


import sys
# sys.stdout.reconfigure(encoding="utf-8")
_first_test = True
def _SKIPTEST_(name):
    if "--skip" in sys.argv:
        return True
    if "--test" not in sys.argv:
        return True
    global _DEBUG_, _first_test
    _DEBUG_ = False
    if "--debug" in sys.argv:
        _DEBUG_ = True
    # return True
    if _first_test:
        _first_test = False
        print("--- START TESTS")
    # return False
    return name in '''
        whenthe whenthe2 whenthe3 whenthemain
        test_load_check
        test_load_random
        test_load_kind_random
        test_store_kind_random
        test_addi_random
        test_slli_random
        test_bitopi_random
        test_sltopi_random
        test_sri_random
        test_lui_random
        test_auipc_random
        test_jalr_random
        test_jal_random
        test_branch_random
        test_fence_random
        test_add_random
        test_sub_random
        test_bitop_random
        test_sltop_random
        test_sll_random
        test_sr_random
        #test_system_random
    '''.split()



def join(*parts):
    if len(parts) == 1:
        if type(parts[0]) is str:
            parts = [parts[0]]
        else:
            parts = parts[0]
    return "\n".join(parts)

def switchon(delta, *cases, **kwargs):
    cases = [g(-delta) + case for case in cases]
    return g(delta) + switch(*cases, **kwargs)

def movea(origin, target):
    return at(origin, move(-origin + target))

def copya(origin, target, temp):
    return at(origin, copy(-origin + temp, -origin + target))

def loopwhilenot(amount, code):
    return c(-amount) + loop(c(amount) + code + c(-amount)) + c(amount)

@pack_class_offsets
class Cont(Struct):
    """A continuation structure with a break cell and a landing zone"""
    break_ = Offset(0)
    land = Offset(range(2))

@pack_class_offsets
class Data(Struct):
    """A structure with data cells"""
    a = Offset()
    b = Offset()
    c = Offset()

@pack_class_offsets
class Frame(Struct):
    """A frame structure with Cont head, a function switch, and data cells"""
    cont = Offset(Cont())
    func = Offset(range(2))
    data = Offset(Data())

    BREAKON = -1

    def switchon(self, deltas, *cases, **kwargs):
        # The pointer must be at some other jump point because the temporary cell
        # is zeroed before each case.
        if size(deltas) != 2:
            raise ValueError("deltas must have length 2")
        if deltas[0] + 1 != deltas[1]:
            raise ValueError("deltas aren't adjacent")
        cases = [
            at(-self.cont.land[0], at(deltas[1], s(0)) + case)
            for case in cases
        ]
        return at(
            self.cont.land[0],
            switchon(-self.cont.land[0] + deltas[0], *cases, **kwargs),
        )

    def save(self, func=None):
        return at(self.func[0], s(func) if func is not None else "")

    def push(self, func=None):
        return g(len(self)) + self.save(func)

    def pop(self, func=0):
        return self.save(func) + g(-len(self))

    def loop(self, code):
        return at(
            self.cont.break_,
            loopwhilenot(self.BREAKON, at(-self.cont.break_, code)),
        )

    def break_(self):
        return at(self.cont.break_, s(self.BREAKON)) + self.save(0)

frame = Frame()
data = frame.data


class Icp437:
    # Differences from Code page 437: nul is space, space is U+2017, del is
    # U+2302, NBSP is U+00A4
    def __init__(self):
        import sys
        self.mapping = self.make_mapping(sys.stdout.encoding)
    @staticmethod
    def make_mapping(encoding):
        out = list(bytes(range(256)).decode("charmap"))
        for i, char in enumerate(" ☺☻♥♦♣♠•◘○◙♂♀♪♫☼►◄↕‼¶§▬↨↑↓→←∟↔▲▼", start=0):
            out[i] = char
        for i, char in enumerate("‗", start=0x20):
            out[i] = char
        for i, char in enumerate("⌂", start=0x7F):
            out[i] = char
        # for i, char in enumerate("]", start=0x5D):
            # out[i] = char
        for i, char in enumerate("ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜ¢£¥₧ƒ", start=0x80):
            out[i] = char
        for i, char in enumerate("áíóúñÑªº¿⌐¬½¼¡«»░▒▓│┤╡╢╖╕╣║╗╝╜╛┐", start=0xA0):
            out[i] = char
        for i, char in enumerate("└┴┬├─┼╞╟╚╔╩╦╠═╬╧╨╤╥╙╘╒╓╫╪┘┌█▄▌▐▀", start=0xC0):
            out[i] = char
        for i, char in enumerate("αßΓπΣσµτΦΘΩδ∞φε∩≡±≥≤⌠⌡÷≈°ˑ·√ⁿ²■¤", start=0xE0):
            out[i] = char
        if encoding == "cp437":
            for i in range(32):
                out[i] = chr(i)
            for i, char in zip(b"\a\b\t\n\r \x7F\xF9\xFF", "abtnr_\x7F∙F"):
                out[i] = char
        return out
    def convert(self, data):
        mapping = self.mapping
        return "".join([mapping[char] for char in data])
icp437 = Icp437().convert

r'''
[ general design ]

___ instr pc x0 x1 x2 ... x31 ___ memory
first space is for decoding the instruction, storing rs1/rs2, extracting immediates, calculations
second space is for potentially storing more state (bump 128 to 256 if stuff is being used)

little endian
no wraparound unfortunately
todo: make all zero instruction an error
ebreak can do the # instruction?
ecall can do i/o depending on some register value?
fence is a noop (no multi threading in brainfuck, all memory reads/writes are immediately visible)
generate exception on misaligned jumps? (check lower 2 bits somehow)

bitwise operations should be unpacked into interleaved bits before operations
probably with 1 2 t s with output into 1? and 2 t being a landing with s being the operation potentially
looping until s is not some constant at the start / end
shifting should be done first by shifting 8 bits at a time (just move bytes over) then the individual bits

                                              46   51      59
                                         _____ijkl___fnIJKLxyzw_
                                        40  44
                        24              M   S12dimm.
                        rs1.rs2.rd..pc..__FN
___-aA1_bB1_cC1_dD1_____
xy-_aAbBcCdDeEfFgGhH__
                        abcdABCD
'''


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
        g(2),
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
        g(2),
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

def MAINLOOP():
    BB, F, N, xyzw, S, x1, x2, xd, imm = DECODE_ARGS()
    return loopuntil(ERROR.f, F, join(  # while F != -1:
        at(F, "#"),
        switch(on=F)(  # switch F:
            at(F, s(ERROR.f)) + g(-2),
            *[[func.f, func() + g(-2)] for func in functions if not hasattr(func, "name")],
        ),
        g(2),
    ))




        # case DECODE:  # rename to INCPC later
            # pc.. => rs2.
            # rs1.[0] += 4
            # F = ADD
            # N = AFTERINCPC
            # land __FN

        # case AFTERINCPC:  # rename to something better later
            # clear x0 register (just move to where x0 is and [-] them)
            # DECODE()

        # default:
            # ...

r'''
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                          +[-
                                          >+<
                                          [-
                                          [
                                          f_\1
                                          [-]>-<
                                         <<
                                          ]
                                          >
                                           [-
                                           f=1
                                          <<
                                          ]
                                           <
                                          ]
                                          >
                                           [-
                                           f=0
                                         <<<
                                        ++decode
                                        >>>
                                        <<<<
                                          <<
                                          ]
                                           <
                                        >>
                                          +]
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
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
        all parts can fit in a byte (decoding crosses boundaries tho)
        from riscv
        VUTSRQPO NMLKJIHG FEDCBA98 76543210 little endian instruction
        6543210 opcode
        7       rd[0] / Stype imm[0] / Btype imm[11]
        BA98    rd[4:1] / SBtype imm[4:1]
        EDC     funct3 / UJtype imm[14:12]
        F       rs1[0] / UJtype imm[15]
        JIHG    rs1[4:1] / UJtype imm[19:16]
        K       rs2[0] / Itype imm[0] / Utype imm[20] / Jtype imm[11]
        NML     rs2[3:1] / IJtype imm[3:1] / Utype imm[23:21]
        O       rs[4] / IJtype imm[4] / Utype imm[24]
        QP      funct7[1:0] / ISBJtype imm[6:5] / Utype imm[26:25]
        UTSR    funct7[5:2] / ISBJtype imm[10:7] / Utype imm[30:27]
        V       funct7[6] / IStype imm[11] / Btype imm[12] / Utype imm[31] / Jtype imm[20]
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
        movea(origins[1], target),
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

# TODO: Maybe optimize by moving landing into _t like below
# __f1_ijkl_IJKLxyzwn  # n is at least space and moved elsewhere during read/writes
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


# lx will load from x and output it to stdout
# sxv will store v at x


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
            + loopwhilenot(-1,
                at(-self._t4[3] + self.num2[3],
                    c(1)
                    + loopdown(at(-self.num2[3] + self._t3[3], c(-1)))
                    + movea(-self.num2[3] + self._t3[3], 0)
                )
                + g(-self._t4[3] + self._t4[2])
            )
            + c(1)
        )

    # move rs1 to abcd
    # move rs2 to ABCD
    # - aA1_ bB1_ cC1_ dD1_ ____
    # if funct7
        # bitwise negate rs2
        # https://esolangs.org/wiki/Brainfuck_algorithms#x%C2%B4_=_not_x_(bitwise)
        # -> >>>> >>>> >>>> >>> +[-<< +[->-<]>[-<+>]<< <+]
    # calculate rs1 + rs2
    def add1to2(self):
        assert self._t3[0] - self.num2[0] == self._t4[0] - self._t3[0]
        return at(self._a,
            c(-1) + g(-self._a + self._t4[3])
            + loopwhilenot(-1,
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
        # print(InterleavedInt().add1to2())
        # -> >>>> >>>> >>>> >>>
        # +[- <<<d
        # [->>+<+[>-]> [[->>>>++<+[>--]>] <+[-<<<<] >] <<<]
        # <+]
        # move ABCD to rd
        # clear msb

    # move rs1 to abcd
    # move rs2 to ABCD
    # - aA1_ bB1_ cC1_ dD1_
    def compare1lessthan2(self):
        assert self._t3[0] - self.num2[0] == self._t4[0] - self._t3[0]
        return at(self._a,
            c(-1) + g(-self._a + self._t4[3])
            + loopwhilenot(-1,
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
                                loopwhilenot(-1,
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
                                loopwhilenot(-1,
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
        # move ABCD to rd
        # print(InterleavedInt().compare1lessthan2())

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
            + loopwhilenot(-1,
                move(-self.bits1[8] + self.bits2[8])
                + bit_func(0, -self.bits1[0] + self.bits2[0])
                + at(-self.bits1[8] + self.bits1[9], loopdown(at(-self.bits1[9] + self.bits1[8], c(2))))
                + g(-self.bits1[8] + self.bits1[7])
            )
            + c(1)
            + g(-self.bits1[0])
            + movea(self.bits1[1], self.byte1)
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


_DEBUG_ = False

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

    class ToyRiscvHead(Struct):
        mem = Offset(MemoryHead(-MemoryHead().data.rwp[0]))  # data is at first 4 cells
        funct_flag = Offset(8)
        x1 = Offset(9)
        x2 = Offset(10)
        xd = Offset(11)
        imm = Offset(range(12,12+4))

        BB = Offset(range(19,19+9))
        B = Offset(BB.offset[1:])

        land = Offset(range(4,4+2))
        F = Offset(6)
        N = Offset(7)

    return RiscvHead()
    # return ToyRiscvHead()

def DECODE_ARGS():
    rhead = DECODE_FORMAT()
    POS = dict(
        BB = range(27,27+9),
        **dict(zip(map(str.strip, "F, N".split(",")), range(42,42+2))),
        xyzw = range(59,59+4),
        **dict(zip(map(str.strip, "S, x1, x2, xd".split(",")), range(44,44+4))),
        imm = range(48,48+4),
    )
    POS = dict(
        BB = range(19,19+9),
        **dict(zip(map(str.strip, "F, N".split(",")), range(6,6+2))),
        xyzw = range(0,0+4),
        **dict(zip(map(str.strip, "S, x1, x2, xd".split(",")), range(8,8+4))),
        imm = range(12,12+4),
    )
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

_INSTR = lambda n: "error" if n == 255 else "load mathi store math branch env auipc lui jalr jal fence".split()[n-1]
def whenthe(func):
    if _SKIPTEST_("whenthe"): return func
    import runbf
    args = DECODE_ARGS()
    _DECODE = func(*args)
    import compressbf; _DECODE = compressbf.compress(","+_DECODE)[1:]
    # _DEBUG_ and print(_DECODE)
    data = DECODE_FORMAT().mem.data.rwp.data
    okay = []
    for i in range(128):
        BB, F, N, xyzw, S, x1, x2, xd, imm = args
        f, n, _x2 = b"".join(map(bytes, runbf.runbf(at(data[0], s(i)) + _DECODE + at([F, N, x2], "."))))
        assert f == 255 or 0 <= f <= 5
        if f != 255:
            assert _INSTR(n)
            assert "IISRBIUUIJI"[n-1] == "RISBUJ"[f]
            okay.append([n, i])
            _DEBUG_ and print(f'{i:07b} {"RISBUJ!"[f if f != 255 else -1]} n={n} {_INSTR(n)} {"i"*_x2}')
        # check that bit 7 isnt used
        assert (f, n, _x2) == tuple(b"".join(bytes(x) for x in runbf.runbf(at(data[0], s(i+128)) + _DECODE + at([F, N, x2], "."))))
    assert sorted([
        [1, 0b_000_00_11],  # load
        [2, 0b_001_00_11],  # mathi
        [3, 0b_010_00_11],  # store
        [4, 0b_011_00_11],  # math
        [5, 0b_110_00_11],  # branch
        [6, 0b_111_00_11],  # env
        [7, 0b_001_01_11],  # auipc
        [8, 0b_011_01_11],  # lui
        [9, 0b_110_01_11],  # jalr
        [10, 0b_110_11_11],  # jal
        [11, 0b_000_11_11],  # fence
    ]) == sorted(okay), okay
    # check that mathi has x2 set to 1
    assert (1,) == tuple(b"".join(map(bytes, runbf.runbf(at(data[0], s(0b0010011)) + _DECODE + at([x2], ".")))))
    return func

@whenthe
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
    LAND = lambda f, on=None: g(-2) if on is None else g(-(f) + (on-2))
    LAUNCH = g(2)
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
                at(N, s(9)),
                at(B, s(0)),
                LAND(F),  # land __FN
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
                        at(N, s(10)),
                        at(B, s(0)),
                        LAND(F),  # land __FN
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
                                at(N, s(11)),
                                LAND(F),  # land __FN
                            ),
                            # case 0:
                            join(
                                # I type
                                # lb (0), lh (1), lw (2), lbu (4), lhu (5)
                                at(F, s(1)),  # F = 1
                                at(N, s(1)),  # N = 1
                                LAND(F),  # land __FN
                                # Note that N starts at 1 since we are using N=0 as an error in the next step
                            ),
                            # case 1:
                            join(
                                # I type
                                # addi (0), slli (1), slti (2), sltiu (3), xori (4), srli/srai (5), ori (6), andi (7)
                                at(F, s(1)),  # F = 1
                                at(N, s(2)),  # N = 2
                                at(x2, s(1)),  # 2 = 1
                                LAND(F),  # land __FN
                            ),
                            # case 2:
                            join(
                                # S type
                                # sb (0), sh (1), sw (2)
                                at(F, s(2)),  # F = 2
                                at(N, s(3)),  # N = 3
                                LAND(F),  # land __FN
                            ),
                            # case 3:
                            join(
                                # R type
                                # add/sub (0), sll (1), slt (2), sltu (3), xor (4), srl/sra (5), or (6), and (7)
                                at(F, s(0)),  # F = 0
                                at(N, s(4)),  # N = 4
                                LAND(F),  # land __FN
                            ),
                            # case 6:
                            [6, join(
                                # B type
                                # beq (0), bne (1), blt (4), bge (5), bltu (6), bgeu (7)
                                at(F, s(3)),  # F = 3
                                at(N, s(5)),  # N = 5
                                LAND(F),  # land __FN
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
                                LAND(F),  # land __FN
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
                                at(N, s(12)),
                                LAND(F),  # land __FN
                            ),
                            # case 1:
                            [1, join(
                                # U type
                                # auipc
                                at(F, s(4)),  # F = 4
                                at(N, s(7)),  # N = 7
                                LAND(F),  # land __FN
                            )],
                            # case 3:
                            [3, join(
                                # U type
                                # lui
                                at(F, s(4)),  # F = 4
                                at(N, s(8)),  # N = 8
                                LAND(F),  # land __FN
                            )],
                            # case 6:
                            [6, join(
                                # I type
                                # jalr
                                at(F, s(1)),  # F = 1
                                at(N, s(9)),  # N = 9
                                LAND(F),  # land __FN
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
                                at(N, s(13)),
                                LAND(F),  # land __FN
                            ),
                            # case 0:
                            join(
                                # U type (its the cheapest path since we ignore all fences)
                                # fence
                                at(F, s(1)),  # F = 1
                                at(N, s(11)),  # N = 11
                                LAND(F),  # land __FN
                            ),
                            # case 6:
                            [6, join(
                                # J type
                                # jal
                                at(F, s(5)),  # F = 5
                                at(N, s(10)),  # N = 10
                                LAND(F),  # land __FN
                            )],
                        ),
                    )],
                ),
            )],
        ),
        LAUNCH,
    )

def whenthe2(func):
    if _SKIPTEST_("whenthe2"): return func
    import runbf
    args = DECODE_ARGS()
    _DECODE = func(*args)
    _DECODE = DECODE_OPCODE_TYPE(*args) + _DECODE
    import compressbf; _DECODE = compressbf.compress(","+_DECODE)[1:]
    instrs = [hex(int.from_bytes(bytes.fromhex(line), "big", signed=True) % 2**32) for line in '''

0000000F

# All of the below were compiled using https://riscvasm.lucasteske.dev/

# https://github.com/racerxdl/riscv-online-asm/blob/3c36eac29f1de46f180e39f6116ef3c87e97b64e/index.html#L56-L73
3e800093
7d008113
c1810193
83018213
3e820293
00010317
fec30313
00430313

# https://marz.utk.edu/my-courses/cosc230/book/example-risc-v-assembly-programs/#strlen
00000293
00a28333
00030303
00030663
00128293
ff1ff06f
00028513
00008067

# https://marz.utk.edu/my-courses/cosc230/book/example-risc-v-assembly-programs/#strcpy
00058283
00550023
00028863
00150513
00158593
fedff06f
00008067

# https://marz.utk.edu/my-courses/cosc230/book/example-risc-v-assembly-programs/#strncpy
00000293
02c2d063
00558333
00030303
00030a63
005503b3
00638023
00128293
fe5ff06f
00c2da63
00550333
00030023
00128293
ff1ff06f
00008067

# https://marz.utk.edu/my-courses/cosc230/book/example-risc-v-assembly-programs/#strrev
# Requires strlen
00000293
00a28333
00030303
00030663
00128293
ff1ff06f
00028513
00008067
ff010113
00113023
00913423
00050493
fd1ff0ef
40155293
00000313
02535663
006483b3
40650e33
fffe0e13
009e0e33
00038e83
000e0f03
01de0023
01e38023
00130313
fd9ff06f
00813483
00013083
01010113
00008067

# https://marz.utk.edu/my-courses/cosc230/book/example-risc-v-assembly-programs/#sum_of_an_integer_array
00000293
00000313
00b35e63
00231393
007503b3
0003a383
007282b3
00130313
fe9ff06f
00028513
00008067

# https://marz.utk.edu/my-courses/cosc230/book/example-risc-v-assembly-programs/#bubble_sort
00000293
00100313
02b35663
00331e13
01c50e33
ff8e3e83
000e3f03
01df5863
00100293
01de3023
ffee3c23
00130313
fd9ff06f
fc0296e3
00008067

# https://marz.utk.edu/my-courses/cosc230/book/advanced/#word_wrap
# Only the greedy_wrap function is here since we cannot link to the stdlib
# Also replaced #define X Y with .equ X, Y
00000293
00000313
00000393
00000e13
00b39c63
002e1f13
00df0f33
007f2023
001e0e13
08c0006f
00031c63
00439f13
00af0f33
008f2303
00138393
fd5ff06f
00439f13
00af0f33
008f2783
00f30fb3
00cfda63
00178793
00f30333
00138393
fb1ff06f
002e1f13
00df0f33
007f2023
001e0e13
02b3da63
40660eb3
01d05c63
00100793
# 03de8eb3  # Unsupported MUL instructions
# 03d78eb3
01d282b3
0180006f
000e8a63
06400793
# 03de8eb3
# 03d78eb3
01d282b3
00000313
f65ff06f
002e1f13
00df0f33
000f2023
00028513
00008067

0003A303
0052d603

    '''.split("\n") if line.strip() and not line.strip().startswith("#")]
    # instrs = instrs[-1:]
    # instrs = ["0b_1111111_00111_00111_001_11100_0100011"]
    # instr = "0b_0100000_00110_00101_101_01010_0010011"
             # 0b_1111111 10001 11111 111 00000 1101111
    SIGNED = lambda x: (x + 2**31) % 2**32 - 2**31
    data = DECODE_FORMAT().mem.data.rwp.data
    # _DEBUG_ and print(_DECODE)
    for instr in instrs:
        BB, F, N, xyzw, S, x1, x2, xd, imm = args
        prefix = "".join(at(i, c(x)) for i, x in zip(data, SIGNED(eval(instr)).to_bytes(4, "little", signed=True)))
        suffix = at([F, N, S,x1,x2,xd, *imm], ".")
        f, n, S,x1,x2,xd, *imm = b"".join(map(bytes, runbf.runbf(prefix + _DECODE + suffix)))
        imm = int.from_bytes(bytes(imm), "little", signed=True) % 2**32
        _DEBUG_ and print(f'f={f} {_INSTR(f)} n={n} S={S} x1={x1} x2={x2} xd={xd} imm={SIGNED(imm)}')
        if f in (1, 2, 6, 9, 11):  # load | mathi | env | jalr | fence I-type
            out_instr = f'0b_{(imm&0xFFF)|(0x400 if S>7 else 0):012b}_{x1:05b}_{S&0x7:03b}_{xd:05b}_{"0000011" if f==1 else "0010011" if f==2 else "1110011" if f==6 else "1100111" if f==9 else "0001111"}'
        elif f == 3:  # store S-type
            out_instr = f'0b_{(imm>>5)&0x7F:07b}_{x2:05b}_{x1:05b}_{S%8:03b}_{imm&0x1F:05b}_0100011'
        elif f == 4:  # math R-type
            out_instr = f'0b_{0x20 if S>7 else 0:07b}_{x2:05b}_{x1:05b}_{S&0x7:03b}_{xd:05b}_0110011'
        elif f == 5:  # branch B-type
            out_instr = f'0b_{((imm>>5)&0x3F)|(((imm>>12)&1)<<6):07b}_{x2:05b}_{x1:05b}_{S&0x7:03b}_{(imm&0x1E)|(imm>>11&1):05b}_1100011'
        elif f in (7, 8):  # auipc | lui U-type
            out_instr = f'0b_{(imm>>12)&0xFFFFF:020b}_{xd:05b}_{"0010111" if f==7 else "0110111"}'
        elif f == 10:  # jal J-type
            out_instr = f'0b_{(imm&0x100000)>>1 | ((imm>>1)&0x3FF)<<9 | ((imm>>11)&1)<<8 | (imm>>12)&0xFF:020b}_{xd:05b}_1101111'
        else:
            out_instr = "error"
        if out_instr != "error":
            assert len(out_instr[2:].replace("_", "")) == 32
        if _DEBUG_ or out_instr == "error" or eval(out_instr) != eval(instr):
            print(f'f={f} {_INSTR(f)} n={n} S={S} x1={x1} x2={x2} xd={xd} imm={SIGNED(imm)}')
            print(out_instr)
            if instr.startswith("0x"):
                print(f'{eval(instr):08x}')
            print(f'0b_{eval(instr):032b}')
            assert out_instr != "error"
            assert eval(out_instr) == eval(instr)
    return func

@whenthe2
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
    LAND = lambda f, on=None: g(-2) if on is None else g(-(f) + (on-2))
    LAUNCH = g(2)
    INFO_BITS = lambda *bits: "".join(move(B[i], 2*[B[i-1]]) for i in reversed(range(8)) if i not in bits)
    # extract parts from instruction according to format
    # 012345 == RISBUJ
    return switch(on=F)(  # switch F:
        # default:
        join(
            # Keep N propagated
            at(F, s(14)),
            ifnonzero(N, at(F, s(0)) + move(N, F)),
            move(F, N),
            at(F, s(ERROR.f)),  # F = ERROR
            at(B, s(0)),
            LAND(F),  # land __FN
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
            move(B[7], 2*[B[5]]), INFO_BITS(0, 1, 6, 7),  # 0, 1, 6 have info
            move(B[0], 16*[x2]),  # if [0]: [0] = 0; 2 += 16
            ifnonzero(B[6], at(S, c(8))),  # if [6]: [6] = 0; S += 8
            # ensure all other bits are 0
            ifnonzero(B[1], at(N, s(0))),  # if [1]: [1] = 0; N = ERROR

            # S=funct3+((funct7&0x20)<<3) 1=rs1 2=rs2 d=rd
            at(F, s(ERROR.f)), at(F-1, s(22)), ifnonzero(N, at(F, s(0)) + move(N, F) + at(F-1, s(0))), move(F-1, N),  # N => F
            LAND(F),  # land __FN
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
            at(F, s(ERROR.f)), at(F-1, s(23)), ifnonzero(N, at(F, s(0)) + move(N, F) + at(F-1, s(0))), move(F-1, N),  # N => F
            LAND(F),  # land __FN
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
            LAND(F),  # land __FN
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
            LAND(F),  # land __FN
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
            LAND(F),  # land __FN
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
            LAND(F),  # land __FN
        ),
    ) + LAUNCH

# MEMMOVRIGHT = 1
# _LOAD_1 = 2

def zipmove(origin, target):
    origin = list(origin)
    target = list(target)
    assert len(origin) == len(target)
    return "".join(move(o, t) for o, t in zip(origin, target))
def whenthe3(func):
    if _SKIPTEST_("whenthe3"): return func
    BB, F, N, xyzw, S, x1, x2, xd, imm = DECODE_ARGS()
    h = DECODE_FORMAT()
    md = h.mem.data
    code = func(*DECODE_ARGS())
    import compressbf; code = compressbf.compress(","+code)[1:]
    prefix = "".join(
        at(i, s(v))
        for i, v in zip(
            # S x1 xd imm.
            [F, S, x1, x2, xd, *imm],
            [1, 5, 6, 0, 7, *[1, 2, 3, 4]],
        )
    )
    prefix += at(list(range(0,64)), ".")
    _parts = 'F, S, x1, x2, xd, md.f, md.n, h.mem_next, *imm, *h.rs1, *h.rs2, *h.rd, *h.mem.i, *md.i'.split(", ")
    suffix = g(-md.f+F) + at([i for part in _parts for i in (eval(part[1:]) if part[0] == "*" else [eval(part)])], ".")
    suffix += at(list(range(0,64)), ".")
    import compressbf; prefix = compressbf.compress(","+prefix)[1:]
    import compressbf; suffix = compressbf.compress(","+suffix)[1:]
    # print(prefix)
    # print("===")
    # print(code)
    # print("===")
    # print(suffix)
    import compressbf; suffix = compressbf.compress(","+suffix)[1:]
    import runbf
    out = memoryview(b"".join(map(bytes, runbf.runbf(prefix + code + suffix))))
    if "." in prefix:
        for _ in range(2):
            _DEBUG_ and print("|", icp437(bytes(out[:32])), "|", sep="")
            out = out[32:]
    for part in _parts:
        if part[0] == "*":
            _info = list(out[0:4])
            out = out[4:]
        else:
            _info = out[0]
            out = out[1:]
        # print(part, "=", _info)
    while out:
        _DEBUG_ and print("|", icp437(bytes(out[:32])), "|", sep="")
        out = out[32:]
    return func

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

@whenthe3
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
    LAND = lambda f, on=None: g(-2) if on is None else g(-(f) + (on-2))
    LAUNCH = g(2)
    INFO_BITS = lambda *bits: "".join(move(B[i], 2*[B[i-1]]) for i in reversed(range(8)) if i not in bits)
    h = DECODE_FORMAT()
    md = h.mem.data
    # do the actual instruction
    return switch(on=F)(  # switch F:
        # default:
        join(
            # F = ERROR
            at(F, s(-1)),
            # break out of main loop?
            # maybe later have a trap
            LAND(F),
        ),
# TODO: minimize head size
#                                               46   51      59
#                                          _____ijkl___fnIJKLxyzw_
#                                         40  44
#                         24              M   S12dimm.
#                         rs1.rs2.rd..pc..__FN
# ___-aA1_bB1_cC1_dD1_____
# xy-_aAbBcCdDeEfFgGhH__
#                         abcdABCD
# [ procedures ]

# to read/write a register
    # memory move right ( pointer = 4*register ; data pointer = 4*register ; n = 2 )
# to read/write an address
    # memory move right ( pointer = 128 ; data pointer = address ; n = 1 )

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
            LAND(F, on=md.f),  # land __fn
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
                    at(h.N, s(24)),
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
            LAND(F, on=md.f),  # land __fn
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
                    at(h.N, s(24)),
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
                    at(h.N, s(25)),
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
            at(h.F, s(ERROR.f)),
            at(h.N, s(7)),
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
            at(h.F, s(ERROR.f)),
            at(h.N, s(8)),
            g(-2),
        )],
    ) + LAUNCH


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
                at(h.N, s(20)),
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
        g(2),

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
        g(2),
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
#                                               46   51      59
#                                          _____ijkl___fnIJKLxyzw_
#                                         40  44
#                         24              M   S12dimm.
#                         rs1.rs2.rd..pc..__FN
# ___-aA1_bB1_cC1_dD1_____
# xy-_aAbBcCdDeEfFgGhH__
    return join(
        at(h.F, s(0)),
        at(h.rd[1], s(2)),
        # at(2*[*h.pc], ".") + at(h.rd[0], "."*8),
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
                at(h.N, s(21)),
                g(-2),
            ),
            join(
                at(md.f, s(MEMMOVRIGHT.f)),
                at(md.n, s(0)),
                at(h.mem_next, s(INCPC.f)),
                g(-h.F + md.f - 2)
            ),
        ),
        g(2),
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
        # at(h.F, s(ERROR.f)) + at(h.N, s(0x69)), ) or (
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
        at(h.rd[1], "#"),
        move(h.rd[1], BBB[0]),
        ifnonzero(BBB[0], join(
            # at(h.rd[2], c(1)),
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
        # at(h.F, s(ERROR.f)) + at(h.N, s(0x69)), ) or (
        ifnonzero(h.rd[1], join(
#                                               46   51      59
#                                          _____ijkl___fnIJKLxyzw_
#                                         40  44
#                         24              M   S12dimm.
#                         rs1.rs2.rd..pc..__FN
# ___-aA1_bB1_cC1_dD1_____
# xy-_aAbBcCdDeEfFgGhH__

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
        # invert all bits
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

def whenthemain():
    if _SKIPTEST_("whenthemain"): return
    h = DECODE_FORMAT(); md = h.mem.data
    code = MAINLOOP()
    import compressbf; code = compressbf.compress(","+code)[1:]
    prefix = suffix = ""
    prefix += "".join(s(v) + "." for v in b'''\
   -aA1_bB1_cC1_dD1_____                M_____ijkl___fnIJKLxyzw_
xy-_aAbBcCdDeEfFgGhH__  rs1.rs2.rd..pc..__FNS12dimm.
''') + s(0)
    # f=1 load n=0 S=2 x1=7 x2=0 xd=6 imm=0
    # lw x6, 0(x7)
    instr = [3, 128+32+3, 3, 0]
    # f=3 store n=0 S=1 x1=7 x2=7 xd=0 imm=-4
    # sh x7, -4(x7)
    instr2 = (((0b_1111111_00111_00111_001_11100_0100011)+2**31)%2**32-2**31).to_bytes(4, "little", signed=True)
    prefix += init([*[0]*64, *[*range(4*7), 20,0,0,0, *range(4*(7+1),4*32)], *instr, *instr2, *[0]*4, *range(8,256)])
    # prefix += at(list(range(0,128)), ".")
    # suffix += at(list(range(0,128)), ".")
    if True:
        prefix = "".join(s(v) + "." for v in b'''\
   -aA1_bB1_cC1_dD1_____                M_____ijkl___fnIJKLxyzw_
xy-_aAbBcCdDeEfFgGhH__  rs1.rs2.rd..pc..__FNS12dimm.
''') + s(0)
        parts = '''
11223344
55667788
9900aabb
ccddeeff
0002a503.text
00428583
0052d603
0072c683
00829703
        '''.split()
        def _get_code(part):
            if type(part) is int:
                return part.to_bytes(5, "little")[:4]
            if part.endswith(".text"):
                return _get_code(part[:-len(".text")])
            if part.startswith("0b"):
                return _get_code(part[len("0b"):])
            if len(part) == 8:
                return _get_code(int(part, base=16))
            raise ValueError(f'cannot decode instruction: {part}')
        prefix += at(64+128, init([x for part in parts for x in _get_code(part)]))
        for i, part in enumerate(parts):
            if part.endswith(".text"):
                prefix += "".join(at(h.pc[_i], s(v)) for _i, v in enumerate(_get_code(4*i)))
                break
    import compressbf; prefix = compressbf.compress(","+prefix)[1:]
    import compressbf; suffix = compressbf.compress(","+suffix)[1:]
    _DEBUG_ and print(prefix)
    _DEBUG_ and print("===")
    _DEBUG_ and print(code)
    _DEBUG_ and print("===")
    _DEBUG_ and print(suffix)
    return
    import runbf
    out = memoryview(b"".join(map(bytes, runbf.runbf(prefix + code + suffix))))
    _DEBUG_ and print("/")
    if "." in prefix:
        for _ in range(4):
            _DEBUG_ and print("|", icp437(bytes(out[:32])), "|", sep="")
            out = out[32:]
        _DEBUG_ and print("\\")
    while out:
        _DEBUG_ and print("|", icp437(bytes(out[:32])), "|", sep="")
        out = out[32:]
    return
whenthemain()

# Used https://riscemu.readthedocs.io/en/latest/riscemu.html to match output
def riscv_test(_check=None, *, name=None, _hex=None, memory=b"", registers=b"", pc=b"", _rng=None, _debug=False, _code=[]):
    if _check is None:
        return lambda _check, *, _kwargs=locals(), **kwargs: riscv_test(**{**_kwargs, **kwargs, "_check": _check})

    if name is None:
        name = _check.__name__

    if _SKIPTEST_(name): return
    h = DECODE_FORMAT(); md = h.mem.data
    HEAD = size(h)
    REGS = 4*32
    assert HEAD == 64
    assert REGS == 128

    if _DEBUG_:
        _debug = True

    prefix = suffix = ""
    rng = None
    seed = _rng
    static_parts = {}
    current_parts = {}
    _last_part = None
    if _hex is not None:
        assert not memory
        def _get_code(part):
            nonlocal rng, seed, _last_part
            if isinstance(part, bytes):
                _debug and print("<", part)
                return part

            if isinstance(part, int):
                _debug and print("<", part)
                return (((part+2**31)%2**32)-2**31).to_bytes(4, "little", signed=True)

            if "#" in part:
                return _get_code(part[:part.index("#")])

            if part.endswith(".text"):
                return _get_code(part[:-len(".text")])

            if ".rng" in part:
                i = part.index(".rng")
                j = i + len(".rng")
                k = 0
                _curlen = ""
                _parentheses = 0
                _state = ""
                while j+k < len(part) and (_parentheses or part[j+k] in (
                    "abcdefghijklmnopqrstuvwxyz"
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                    "0123456789"
                    "!@$%^&*,:;"
                    "("
                )):
                    char = part[j+k]
                    if _parentheses:
                        if char == ")":
                            _parentheses -= 1
                            if not _parentheses:
                                _state = [_state]
                        else:
                            _state += char
                    elif char == "(":
                        _parentheses = 1
                    else:
                        if char in "0123456789":
                            _curlen += char
                        else:
                            if char in static_parts:
                                if _state:
                                    raise ValueError(f'cannot apply state on previous parts: {part, char, _state}')
                                if _curlen and int(_curlen) != len(static_parts[char]["value"]):
                                    raise ValueError(f'length does not match previous part: {part, char, _curlen}')
                                current_parts[char] = static_parts[char]
                            else:
                                if _curlen:
                                    assert int(_curlen) != 0
                                cpart = current_parts[char] = dict(value="", morechars=int(_curlen) if _curlen else -1)
                                if _state:
                                    options = {}
                                    current_key = None
                                    for _s in map(str.strip, _state[0].split(";")):
                                        if not _s:
                                            continue
                                        if _s.startswith("="):
                                            current_key = _s[1:].strip()
                                            options.setdefault(current_key, [])
                                        elif current_key is None:
                                            raise ValueError(f'missing state key: {part}')
                                        else:
                                            options[current_key].append(_s)
                                    _state = ""
                                    if "clear" in options:
                                        if options["clear"]:
                                            for char in options["clear"]:
                                                del static_parts[char]
                                                del current_parts[char]
                                        else:
                                            static_parts.clear()
                                            current_parts.clear()
                                    if "pick" in options:
                                        if rng is None:
                                            import sys, random
                                            seed = random.randrange(sys.maxsize) if seed is None else seed
                                            rng = random.Random(seed)
                                        for _ in range(50):
                                            picked = rng.choice(options["pick"])
                                            _debug and print("<", "picked", picked, "for", char)
                                            cpart["morechars"] = 0
                                            if picked.startswith("-"):
                                                old_last_part, _last_part = _last_part, -1
                                                _get_code(picked[1:])
                                                cpart["value"] = _last_part
                                                _last_part = old_last_part
                                            else:
                                                cpart["value"] = picked
                                            if _curlen:
                                                assert int(_curlen) == len(_last_part), (_curlen, _last_part)
                                            if "disallow" in options:
                                                for _disallow in options["disallow"]:
                                                    if _disallow.startswith("-"):
                                                        for _ in range(50):
                                                            old_last_part, _last_part = _last_part, -1
                                                            _get_code(_disallow[1:])
                                                            if _last_part != cpart["value"]:
                                                                break
                                                            _last_part = old_last_part
                                                        else:
                                                            break
                                                    else:
                                                        if _disallow == cpart["value"]:
                                                            break
                                                else:
                                                    break
                                                continue
                                            break
                                        else:
                                            raise ValueError(f'could not find satisfying option: {part}')
                                    elif "disallow" in options:
                                        raise ValueError(f'disallow missing corresponding pick option: {part}')
                            _curlen = ""
                    k += 1
                if len(_curlen) > 0:
                    raise ValueError(f'missing character after size: {_curlen_}')
                if _parentheses:
                    raise ValueError(f'unclosed parentheses: {part}')
                if _state:
                    raise ValueError(f'unapplied state: {part}')
                _debug and print("<", "rng", current_parts)
                current_parts[""] = True
                return _get_code(part[:i]+part[j+k:])

            if part.startswith("0b"):
                part = part[2:]
                if _last_part != -1:
                    assert len(part.replace("_", "")) % 8 == 0, part
                if "?" in part or current_parts:
                    if rng is None:
                        import sys, random
                        seed = random.randrange(sys.maxsize) if seed is None else seed
                        rng = random.Random(seed)
                    chars = list(part)
                    indices = {}
                    for i, char in enumerate(chars):
                        if char == "?":
                            chars[i] = rng.choice("01")
                        elif char in "_01":
                            continue
                        else:
                            indices.setdefault(char, 0)
                            if char in current_parts and current_parts[char].get("morechars"):
                                current_parts[char]["morechars"] -= 1
                                current_parts[char]["value"] += rng.choice("01")
                            if char in current_parts:
                                cpart = current_parts[char]
                            elif char in static_parts:
                                cpart = static_parts[char]
                            else:
                                raise ValueError(f'missing character in .rng: {char}')
                            chars[i] = cpart["value"][indices[char] % len(cpart["value"])]
                            indices[char] += 1
                    part = "".join(chars)
                _debug and print("<", f'0b{part}')
                if "_" in part:
                    part = part.replace("_", "")
                if _last_part == -1:
                    _last_part = part
                return int(part, base=2).to_bytes(len(part)//8 + 2, "little")[:-2]

            if part.startswith("0x") or len(part) == 8:
                has_leading = part.startswith("0x")
                if has_leading:
                    part = part[2:]
                if _last_part != -1:
                    assert len(part.replace("_", "")) % 2 == 0, part
                if "?" in part or current_parts:
                    if rng is None:
                        import sys, random
                        seed = random.randrange(sys.maxsize) if seed is None else seed
                        rng = random.Random(seed)
                    chars = list(part)
                    indices = {}
                    for i, char in enumerate(chars):
                        if char == "?":
                            chars[i] = rng.choice("0123456789ABCDEF")
                        elif char in "_0123456789abcdefABCDEF":
                            continue
                        else:
                            indices.setdefault(char, 0)
                            if char in current_parts and current_parts[char].get("morechars"):
                                current_parts[char]["morechars"] -= 1
                                current_parts[char]["value"] += rng.choice("0123456789ABCDEF")
                            if char in current_parts:
                                cpart = current_parts[char]
                            elif char in static_parts:
                                cpart = static_parts[char]
                            else:
                                raise ValueError(f'undeclared character in .rng: {char}')
                            chars[i] = cpart["value"][indices[char] % len(cpart["value"])]
                            indices[char] += 1
                    part = "".join(chars)
                _debug and print("<", f'0x{part}' if has_leading else part)
                if "_" in part:
                    part = part.replace("_", "")
                if _last_part == -1:
                    _last_part = part
                return int(part, base=16).to_bytes(len(part)//2 + 2, "little")[:-2]

            if not part:
                return b""

            raise ValueError(f'cannot decode instruction: {part}')

        memory = bytearray()
        for part in _hex.split():
            _debug and print(" >", part, sep="")
            if not pc and part.endswith(".text"):
                pc = _get_code(len(memory))
            try:
                _memcode = _get_code(part)
            except:
                print(f'error while decoding: {part}')
                raise
            else:
                if _memcode:
                    # _debug and print("<<", _memcode, sep="")
                    memory += _memcode
                    static_parts.update(current_parts)
                    current_parts.clear()

    if pc:
        _olddebug, _debug = _debug, False
        prefix += at(h.pc[0], init(_get_code(pc)))
        _debug = _olddebug
    if registers:
        prefix += at(HEAD, init(registers))
    if memory:
        prefix += at(HEAD+REGS, init(memory))

    if not _code:
        code = MAINLOOP()
        import compressbf; code = compressbf.compress(","+code)[1:]
        _code.append(code)
    code = _code[0]

    _show_range = range(HEAD+REGS+max(128,len(memory)))
    prefix += at(list(_show_range), ".")
    suffix += at(list(_show_range), ".")

    import compressbf; prefix = compressbf.compress(","+prefix)[1:]
    import compressbf; suffix = compressbf.compress(","+suffix)[1:]

    SIZE = 16
    assert prefix.count(".") % SIZE == 0, prefix.count(".")


    print(f'/{name} seed={seed}')
    if _DEBUG_:
        if prefix:
            print(prefix)
            print("===")
        print(code)
        print("===")
        if suffix:
            print(suffix)
            print("===")
    import runbf; out = memoryview(b"".join(map(bytes, runbf.runbf(prefix + code + suffix))))
    inmem = outmem = None

    if "." in prefix:
        inmem = bytearray()
        for _ in range(prefix.count(".") // SIZE):
            if _DEBUG_:
                _DEBUG_ and print(f'|{icp437(bytes(out[:SIZE]))}| {" ".join(f"{o:02X}" for o in out[:SIZE])}')
            inmem += bytes(out[:SIZE])
            out = out[SIZE:]
        if _DEBUG_:
            print("\\")
    if len(out):
        outmem = bytearray()
        while out:
            if _DEBUG_:
                _DEBUG_ and print(f'|{icp437(bytes(out[:SIZE]))}| {" ".join(f"{o:02X}" for o in out[:SIZE])}')
            outmem += out[:SIZE]
            out = out[SIZE:]

    # check any invariants
    if _check is not None:
        try:
            if _check(h=h, md=md, HEAD=HEAD, REGS=REGS, inmem=inmem, outmem=outmem):
                raise RuntimeError("check failed")
        except:
            print(f'! check failed: {name} seed={seed} {_check}')
            raise
        print("passed")

    return

# 6856697699274175806
@riscv_test(_hex='''
????????
????????
????????
????????
0002a503.text #_lw_a0,_0(t0)
00428583      #_lb_a1,_4(t0)
0052d603      #_lhu_a2,_5(t0)
0072c683      #_lbu_a3,_7(t0)
00829703      #_lh_a4,_8(t0)
''')
def test_load_check(HEAD, REGS, inmem, outmem, **_):
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    r = lambda i: FB(outmem[HEAD:][4*i:4*(i+1)])
    ir = lambda i: FB(inmem[HEAD:][4*i:4*(i+1)])
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    assert r(10) == FB(im[0:4])
    assert r(11) == FB(im[4:5])
    assert r(12) == FBU(im[5:7])
    assert r(13) == FBU(im[7:8])
    assert r(14) == FB(im[8:10])

@riscv_test(_hex='''
????????
????????
????????
????????
00000000
00000000
00000000
00000000
.text
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1)A
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1)B
.rng(=pick;000;001;010;100;101)L
.rng(=pick;000;001;010)S
0b_00000000????_BBBBB_LLL_AAAAA_0000011.rngABLS #load
0b_00000000????_00000_LLL_AAAAA_0000011.rngABLS #load
0b_00000001????_00000_LLL_AAAAA_0000011.rngABLS #load
0b_000000010???_00000_010_AAAAA_0000011.rngABLS #load
''')
def test_load_random(HEAD, REGS, inmem, outmem, **_):
    assert inmem[HEAD:] == outmem[HEAD:]

@riscv_test(_hex='''
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1)A
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1;=disallow;-0bAAAAA.rng)B
.rng(=pick;000;001;010;100;101)L
.rng(=pick;000;001;010)S
.rng(=pick;-0b??)X
.rng(=pick;-0b??)Y
????????
????????
????????
????????
00000000
00000004
00000008
0000000C
0b000AAAAA.rng
0b000BBBBB.rng
0b00000LLL.rng
0b00000SSS.rng
0b000000XX.rng
0b000000YY.rng
.text
0b_00000001XX00_00000_000_AAAAA_0000011.rng #load
0b_0000000000YY_AAAAA_LLL_BBBBB_0000011.rng #load
''')
def test_load_kind_random(HEAD, REGS, inmem, outmem, **_):
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    r = lambda i: FB(outmem[HEAD:][4*i:4*(i+1)])
    ir = lambda i: FB(inmem[HEAD:][4*i:4*(i+1)])
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    rA = im[32]
    rB = im[33]
    load_kind = im[34]
    store_kind = im[35]
    xx = im[36]
    yy = im[37]
    assert rB != rA
    assert rA in range(1, 32), rA
    assert r(rA) in [0, 4, 8, 12], r(rA)
    assert 4*xx == r(rA)
    a = r(rA) + yy
    assert rB in range(1, 32), rB
    if load_kind == 0:  # byte
        assert r(rB) == FB(im[a:a+1])
    elif load_kind == 1:  # half
        assert r(rB) == FB(im[a:a+2])
    elif load_kind == 2:  # word
        assert r(rB) == FB(im[a:a+4])
    elif load_kind == 4:  # byte unsigned
        assert r(rB) == FBU(im[a:a+1])
    elif load_kind == 5:  # half unsigned
        assert r(rB) == FBU(im[a:a+2])
    else:
        assert False, f'unknown load kind: {load_kind}'


@riscv_test(_hex='''
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1)A
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1;=disallow;-0bAAAAA.rng)B
.rng(=pick;000;001;010;100;101)L
.rng(=pick;000;001;010)S
.rng(=pick;-0b??)X
.rng(=pick;-0b??)Y
????????
????????
????????
????????
00000000
00000004
00000008
0000000C
0b000AAAAA.rng
0b000BBBBB.rng
0b00000LLL.rng
0b00000SSS.rng
0b000000XX.rng
0b000000YY.rng
.text
0b_00000001XX00_00000_000_AAAAA_0000011.rng #load
0b_0000000000YY_AAAAA_010_BBBBB_0000011.rng #load
0b_0000000_BBBBB_AAAAA_SSS_10000_0100011.rng #store
''')
def test_store_kind_random(HEAD, REGS, inmem, outmem, **_):
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    rb = lambda i: outmem[HEAD:][4*i:4*(i+1)]
    r = lambda i: FB(rb(i))
    ir = lambda i: FB(inmem[HEAD:][4*i:4*(i+1)])
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    rA = im[32]
    rB = im[33]
    load_kind = im[34]
    store_kind = im[35]
    xx = im[36]
    yy = im[37]
    assert rB != rA
    assert rA in range(1, 32), rA
    assert r(rA) in [0, 4, 8, 12], r(rA)
    assert 4*xx == r(rA)
    a = 16+xx*4
    assert rB in range(1, 32), rB
    if store_kind == 0:  # byte
        assert rb(rB)[:1] == om[a:a+1], (rb(rB)[:1], om[a:a+1])
        om[a:a+1] = im[a:a+1]
    elif store_kind == 1:  # half
        assert rb(rB)[:2] == om[a:a+2], (rb(rB)[:2], om[a:a+2])
        om[a:a+2] = im[a:a+2]
    elif store_kind == 2:  # word
        assert rb(rB)[:4] == om[a:a+4], (rb(rB)[:4], om[a-4:a+4])
        om[a:a+4] = im[a:a+4]
    else:
        assert False, f'unknown store kind: {store_kind}'
    assert im == om


@riscv_test(_hex='''
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1)A
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1;=disallow;-0bAAAAA.rng)B
XXXXXXXX.rng(=pick;00000000;FFFFFFFF;7FFFFFFF;80000000;-0x????????)X
0b_FFFFFFFFFFFFFFFFFFFF_FYYYYYYYYYYY.rng1FY
00000000
.text
0b_000000000000_00000_010_AAAAA_0000011.rng #load
0b_FYYYYYYYYYYY_AAAAA_000_BBBBB_0010011.rng #addi
0b_0000000_BBBBB_00000_010_01000_0100011.rng #store
''')
def test_addi_random(HEAD, REGS, inmem, outmem, **_):
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    wraparound = lambda i, x: (x + 2**(i*8-1)) % 2**(i*8) - 2**(i*8-1)
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    assert wraparound(4, FB(om[0:4]) + FB(om[4:8])) == FB(om[8:12]), [FB(om[0:4]), FB(om[4:8]), FB(om[8:12])]


@riscv_test(_hex='''
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1)A
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1;=disallow;-0bAAAAA.rng)B
XXXXXXXX.rngX
0b_00000000000000000000_0000000YYYYY.rng(=pick;00000;11111;-0b??000;-0b?????)Y
00000000
.text
0b_000000000000_00000_010_AAAAA_0000011.rng #load
0b_0000000YYYYY_AAAAA_001_BBBBB_0010011.rng #slli
0b_0000000_BBBBB_00000_010_01000_0100011.rng #store
''')
def test_slli_random(HEAD, REGS, inmem, outmem, **_):
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    wraparound = lambda i, x: (x + 2**(i*8-1)) % 2**(i*8) - 2**(i*8-1)
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    assert wraparound(4, FB(om[0:4]) << FB(om[4:8])) == FB(om[8:12]), [FB(om[0:4]), FB(om[4:8]), FB(om[8:12])]


@riscv_test(_hex='''
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1)A
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1;=disallow;-0bAAAAA.rng)B
.rng(=pick;100;110;111)O
XXXXXXXX.rngX
0b_FFFFFFFFFFFFFFFFFFFF_FYYYYYYYYYYY.rng1FY
00000000
0b00000OOO.rng
.text
0b_000000000000_00000_010_AAAAA_0000011.rng #load
0b_FYYYYYYYYYYY_AAAAA_OOO_BBBBB_0010011.rng #bitopi
0b_0000000_BBBBB_00000_010_01000_0100011.rng #store
''')
def test_bitopi_random(HEAD, REGS, inmem, outmem, **_):
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    _op = om[12]
    if _op == 4:  # xor
        op = "^"
    elif _op == 6:  # or
        op = "|"
    elif _op == 7:  # and
        op = "&"
    else:
        assert False, f'unknown bit operation kind: {_op}'
    assert eval(f'{FBU(om[0:4])} {op} {FBU(om[4:8])}') == FBU(om[8:12]), [op, FBU(om[0:4]), FBU(om[4:8]), FBU(om[8:12])]

@riscv_test(_hex='''
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1)A
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1;=disallow;-0bAAAAA.rng)B
.rng(=pick;010;011)O
00000XXX.rngX
0b_FFFFFFFFFFFFFFFFFFFF_FYYYYYYYYYYY.rng1FY
00000000
0b00000OOO.rng
.text
0b_000000000000_00000_010_AAAAA_0000011.rng #load
0b_FYYYYYYYYYYY_AAAAA_OOO_BBBBB_0010011.rng #sltopi
0b_0000000_BBBBB_00000_010_01000_0100011.rng #store
''')
def test_sltopi_random(HEAD, REGS, inmem, outmem, **_):
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    _op = om[12]
    if _op == 2:  # slt signed
        fb = FB
    elif _op == 3:  # slt unsigned
        fb = FBU
    else:
        assert False, f'unknown slt kind: {_op}'
    assert int(fb(om[0:4]) < fb(om[4:8])) == FBU(om[8:12]), ["signed" if _op == 2 else "unsigned", fb(om[0:4]), fb(om[4:8]), FBU(om[8:12])]

@riscv_test(_hex='''
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1)A
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1;=disallow;-0bAAAAA.rng)B
XXXXXXXX.rngX
0b_00000000000000000000_0000000YYYYY.rng(=pick;00000;11111;-0b??000;-0b?????)Y
00000000
0b_0000000F.rngF
.text
0b_000000000000_00000_010_AAAAA_0000011.rng #load
0b_0F00000YYYYY_AAAAA_101_BBBBB_0010011.rng #sropi
0b_0000000_BBBBB_00000_010_01000_0100011.rng #store
''')
def test_sri_random(HEAD, REGS, inmem, outmem, **_):
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    _op = om[12]
    if _op == 0:  # srl
        fb = FBU
    elif _op == 1:  # sra
        fb = FB
    else:
        assert False, f'unknown srop kind: {_op}'
    assert fb(om[0:4]) >> fb(om[4:8]) == fb(om[8:12]), ["logical" if _op == 0 else "arithmetic", fb(om[0:4]), fb(om[4:8]), fb(om[8:12])]

@riscv_test(_hex='''
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1)A
0b_XXXXXXXXXXXXXXXXXXXX000000000000.rngX
00000000
.text
0b_XXXXXXXXXXXXXXXXXXXX_AAAAA_0110111.rng #lui
0b_0000000_AAAAA_00000_010_00100_0100011.rng #store
''')
def test_lui_random(HEAD, REGS, inmem, outmem, **_):
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    assert FB(om[0:4]) == FB(om[4:8]), [FB(om[0:4]), FB(om[4:8])]

@riscv_test(_hex='''
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1)A
0b_XXXXXXXXXXXXXXXXXXXX000000000000.rngX
00000000
.text
0b_XXXXXXXXXXXXXXXXXXXX_AAAAA_0010111.rng #auipc
0b_0000000_AAAAA_00000_010_00100_0100011.rng #store
''')
def test_auipc_random(HEAD, REGS, inmem, outmem, **_):
    h = DECODE_FORMAT()
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    assert FB(om[0:4]) + FB(inmem[h.pc[0]:h.pc[0]+len(h.pc)]) == FB(om[4:8]), [FB(om[0:4]), FB(inmem[h.pc[0]:h.pc[0]+len(h.pc)]), FB(om[4:8])]

@riscv_test(_hex='''
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1)A
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1;=disallow;-0bAAAAA.rng)B
0b_1XXXXXXX.rngX
0b_YYYYYYYY.rngY
0b_000AAAAA.rngA
0b_000BBBBB.rngB
.text
0b_000000000000_00000_100_AAAAA_0000011.rng #load
0b_0000YYYYYYYY_AAAAA_000_BBBBB_1100111.rng #jalr
''')
def test_jalr_random(HEAD, REGS, inmem, outmem, **_):
    h = DECODE_FORMAT()
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    x, y, a, b = om[0:4]
    assert 8 + 4 == FBU(outmem[HEAD:][b*4:][:4]), [12, FBU(outmem[HEAD:][b*4:][:4])]
    assert x + y == FBU(outmem[h.pc[0]:][:4]), [x, y, FBU(outmem[h.pc[0]:][:4])]

@riscv_test(_hex='''
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1)A
.rng(=pick;-0b1????;-0b?1???;-0b??1??;-0b???1?;-0b????1;=disallow;-0bAAAAA.rng)B
0b_1XXXXXXX.rngX
0b_YYYYYYY0.rngY
0b_000AAAAA.rngA
0b_000BBBBB.rngB
.text
0b_000000000000_00000_100_AAAAA_0000011.rng #load
0b_0000YYYYYYY0_00000_000_AAAAA_1101111.rng #jal
''')
def test_jal_random(HEAD, REGS, inmem, outmem, **_):
    h = DECODE_FORMAT()
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    x, y, a, b = om[0:4]
    assert 8 + 4 == FBU(outmem[HEAD:][a*4:][:4]), [12, FBU(outmem[HEAD:][a*4:][:4])]
    assert 8 + y == FBU(outmem[h.pc[0]:][:4]), [8, y, FBU(outmem[h.pc[0]:][:4])]

@riscv_test(_hex='''
.rng(=pick;-0b?????;=disallow;00000)A
.rng(=pick;-0b?????;=disallow;00000;-0bAAAAA.rng)B
.rng(=pick;000;001;100;101;110;111)O
XXXXXXXX.rng1X
YYYYYYYY.rng(=pick;-0xXXXXXXXX.rng;-0xZZZZZZZZ.rng1Z)Y
00000001
0b00000OOO.rng
.text
0b_000000000000_00000_010_AAAAA_0000011.rng #load
0b_000000000100_00000_010_BBBBB_0000011.rng #load
0b_0000000_BBBBB_AAAAA_OOO_01100_1100011.rng #branch
0b_0000000_00000_00000_010_01000_0100011.rng #store
''')
def test_branch_random(HEAD, REGS, inmem, outmem, **_):
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    _op = om[12]
    fb = FB
    if _op == 0: op = "=="
    elif _op == 1: op = "!="
    elif _op == 4: op = "<"
    elif _op == 5: op = ">="
    elif _op == 6: op = "<"; fb = FBU
    elif _op == 7: op = ">="; fb = FBU
    else:
        assert False, f'unknown branch kind: {_op}'
    assert int(eval(f'{fb(om[0:4])} {op} {fb(om[4:8])}')) == FBU(om[8:12]), [op, "signed" if fb is FB else "unsigned", fb(om[0:4]), fb(om[4:8]), FBU(om[8:12])]

@riscv_test(_hex='''
.rng(=pick;-0b?????;=disallow;00000)A
.rng(=pick;-0b?????;=disallow;00000;-0bAAAAA.rng)B
0b_000000000000_00000_000_00000_0010011 #noop
0b_FXXXXXXXXXXX_BBBBB_000_AAAAA_0001111.rng1FX #fence
0b_FFFFFFFFFFFFFFFFFFFF_FXXXXXXXXXXX.rng
0b_000AAAAA.rng
0b_000BBBBB.rng
''')
def test_fence_random(HEAD, REGS, inmem, outmem, **_):
    h = DECODE_FORMAT(); md = h.mem.data
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    oldpc = FB(inmem[h.pc[0]:][:4])
    newpc = FB(outmem[h.pc[0]:][:4])
    assert oldpc + 4 == newpc
    assert outmem[h.F] == ERROR.f % 256
    assert outmem[h.N] == 8 % 256
    assert FB(outmem[h.imm[0]:][:4]) == FB(outmem[HEAD+REGS:][8:][:4])
    assert outmem[h.xd] == outmem[HEAD+REGS:][12]
    assert outmem[h.x1] == outmem[HEAD+REGS:][13]

@riscv_test(_hex='''
.rng(=pick;-0b?????;=disallow;00000)A
.rng(=pick;-0b?????;=disallow;00000;-0bAAAAA.rng)B
XXXXXXXX.rng(=pick;00000000;FFFFFFFF;7FFFFFFF;80000000;-0x????????)X
YYYYYYYY.rng(=pick;00000000;FFFFFFFF;7FFFFFFF;80000000;-0x????????)Y
00000000
.text
0b_000000000000_00000_010_AAAAA_0000011.rng #load
0b_000000000100_00000_010_BBBBB_0000011.rng #load
0b_0000000_BBBBB_AAAAA_000_BBBBB_0110011.rng #add
0b_0000000_BBBBB_00000_010_01000_0100011.rng #store
''')
def test_add_random(HEAD, REGS, inmem, outmem, **_):
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    wraparound = lambda i, x: (x + 2**(i*8-1)) % 2**(i*8) - 2**(i*8-1)
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    assert wraparound(4, FB(om[0:4]) + FB(om[4:8])) == FB(om[8:12]), [FB(om[0:4]), FB(om[4:8]), FB(om[8:12])]

@riscv_test(_hex='''
.rng(=pick;-0b?????;=disallow;00000)A
.rng(=pick;-0b?????;=disallow;00000;-0bAAAAA.rng)B
XXXXXXXX.rng(=pick;00000000;FFFFFFFF;7FFFFFFF;80000000;-0x????????)X
YYYYYYYY.rng(=pick;00000000;FFFFFFFF;7FFFFFFF;80000000;-0x????????)Y
00000000
.text
0b_000000000000_00000_010_AAAAA_0000011.rng #load
0b_000000000100_00000_010_BBBBB_0000011.rng #load
0b_0100000_BBBBB_AAAAA_000_BBBBB_0110011.rng #sub
0b_0000000_BBBBB_00000_010_01000_0100011.rng #store
''')
def test_sub_random(HEAD, REGS, inmem, outmem, **_):
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    wraparound = lambda i, x: (x + 2**(i*8-1)) % 2**(i*8) - 2**(i*8-1)
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    assert wraparound(4, FB(om[0:4]) - FB(om[4:8])) == FB(om[8:12]), [FB(om[0:4]), FB(om[4:8]), wraparound(4, FB(om[0:4]) - FB(om[4:8])), FB(om[8:12])]

@riscv_test(_hex='''
.rng(=pick;-0b?????;=disallow;00000)A
.rng(=pick;-0b?????;=disallow;00000;-0bAAAAA.rng)B
.rng(=pick;100;110;111)O
XXXXXXXX.rng(=pick;00000000;FFFFFFFF;-0x????????)X
YYYYYYYY.rng(=pick;00000000;FFFFFFFF;-0x????????)Y
00000000
0b00000OOO.rng
.text
0b_000000000000_00000_010_AAAAA_0000011.rng #load
0b_000000000100_00000_010_BBBBB_0000011.rng #load
0b_0000000_BBBBB_AAAAA_OOO_BBBBB_0110011.rng #bitop
0b_0000000_BBBBB_00000_010_01000_0100011.rng #store
''')
def test_bitop_random(HEAD, REGS, inmem, outmem, **_):
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    _op = om[12]
    if _op == 4:  # xor
        op = "^"
    elif _op == 6:  # or
        op = "|"
    elif _op == 7:  # and
        op = "&"
    else:
        assert False, f'unknown bit operation kind: {_op}'
    assert eval(f'{FBU(om[0:4])} {op} {FBU(om[4:8])}') == FBU(om[8:12]), [op, FBU(om[0:4]), FBU(om[4:8]), FBU(om[8:12])]

@riscv_test(_hex='''
.rng(=pick;-0b?????;=disallow;00000)A
.rng(=pick;-0b?????;=disallow;00000;-0bAAAAA.rng)B
.rng(=pick;010;011)O
XXXXXXXX.rng(=pick;00000000;FFFFFFFF;7FFFFFFF;80000000;-0x????????)X
YYYYYYYY.rng(=pick;00000000;FFFFFFFF;7FFFFFFF;80000000;-0x????????)Y
00000000
0b00000OOO.rng
.text
0b_000000000000_00000_010_AAAAA_0000011.rng #load
0b_000000000100_00000_010_BBBBB_0000011.rng #load
0b_0000000_BBBBB_AAAAA_OOO_BBBBB_0110011.rng #sltop
0b_0000000_BBBBB_00000_010_01000_0100011.rng #store
''')
def test_sltop_random(HEAD, REGS, inmem, outmem, **_):
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    _op = om[12]
    if _op == 2:  # slt signed
        fb = FB
    elif _op == 3:  # slt unsigned
        fb = FBU
    else:
        assert False, f'unknown slt kind: {_op}'
    assert int(fb(om[0:4]) < fb(om[4:8])) == FBU(om[8:12]), ["signed" if _op == 2 else "unsigned", fb(om[0:4]), fb(om[4:8]), FBU(om[8:12])]

@riscv_test(_hex='''
.rng(=pick;-0b?????;=disallow;00000)A
.rng(=pick;-0b?????;=disallow;00000;-0bAAAAA.rng)B
.rng(=pick;-0b_00000000_00000000_00000000_????????;-0b_????????_????????_????????_????????;-0b_FZZZZZZZ_ZZZZZZZZ_ZZZZZZZZ_ZZZZZZZZ.rngF1Z)X
.rng(=pick;-0b_00000000_00000000_00000000_000?????;-0b_????????_????????_????????_????????;-0b_GWWWWWWW_WWWWWWWW_WWWWWWWW_WWWWWWWW.rngG1W)Y
0b_XXXXXXXX_XXXXXXXX_XXXXXXXX_XXXXXXXX.rng
0b_YYYYYYYY_YYYYYYYY_YYYYYYYY_YYYYYYYY.rng
00000000
.text
0b_000000000000_00000_010_AAAAA_0000011.rng #load
0b_000000000100_00000_010_BBBBB_0000011.rng #load
0b_0000000_BBBBB_AAAAA_001_BBBBB_0110011.rng #sll
0b_0000000_BBBBB_00000_010_01000_0100011.rng #store
''')
def test_sll_random(HEAD, REGS, inmem, outmem, **_):
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    wraparound = lambda i, x: (x + 2**(i*8-1)) % 2**(i*8) - 2**(i*8-1)
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    a = FB(om[0:4])
    b = FBU(om[4:8])
    r = FB(om[8:12])
    b32 = b % 32
    assert wraparound(4, a << b32) == r, [wraparound(4, a << b32), a, b, b32, r]

@riscv_test(_hex='''
.rng(=pick;-0b?????;=disallow;00000)A
.rng(=pick;-0b?????;=disallow;00000;-0bAAAAA.rng)B
.rng(=pick;-0b_00000000_00000000_00000000_????????;-0b_????????_????????_????????_????????;-0b_FZZZZZZZ_ZZZZZZZZ_ZZZZZZZZ_ZZZZZZZZ.rngF1Z)X
.rng(=pick;-0b_00000000_00000000_00000000_000?????;-0b_????????_????????_????????_????????;-0b_GWWWWWWW_WWWWWWWW_WWWWWWWW_WWWWWWWW.rngG1W)Y
0b_XXXXXXXX_XXXXXXXX_XXXXXXXX_XXXXXXXX.rng
0b_YYYYYYYY_YYYYYYYY_YYYYYYYY_YYYYYYYY.rng
00000000
0b_0000000F.rngF
.text
0b_000000000000_00000_010_AAAAA_0000011.rng #load
0b_000000000100_00000_010_BBBBB_0000011.rng #load
0b_0F00000_BBBBB_AAAAA_101_BBBBB_0110011.rng #srop
0b_0000000_BBBBB_00000_010_01000_0100011.rng #store
''')
def test_sr_random(HEAD, REGS, inmem, outmem, **_):
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    om = outmem[HEAD+REGS:]
    im = inmem[HEAD+REGS:]
    _op = om[12]
    if _op == 0:  # srl
        fb = FBU
    elif _op == 1:  # sra
        fb = FB
    else:
        assert False, f'unknown srop kind: {_op}'
    a = fb(om[0:4])
    b = FBU(om[4:8])
    r = fb(om[8:12])
    b32 = b % 32
    assert a >> b32 == r, ["logical" if _op == 0 else "arithmetic", a >> b32, a, b, b32, r]

@riscv_test(_hex='''
0b_0000000_00000_00000_000_00000_0010011 #noop
0b_0000000_0000F_00000_000_00000_1110011.rngF #ecall/ebreak
0b_0000000F.rng
0x5D
0x7F
''')
def test_system_random(HEAD, REGS, inmem, outmem, **_):
    h = DECODE_FORMAT(); md = h.mem.data
    FB = lambda _bytes: int.from_bytes(_bytes, "little", signed=True)
    FBU = lambda _bytes: int.from_bytes(_bytes, "little", signed=True) % 2**(8*len(_bytes))
    oldpc = FB(inmem[h.pc[0]:][:4])
    newpc = FB(outmem[h.pc[0]:][:4])
    assert oldpc + 4 == newpc
    assert outmem[h.F] == ERROR.f % 256
    assert outmem[h.N] == 7 % 256
    assert outmem[h.x2] == outmem[HEAD+REGS:][8]

if not _first_test:
    print("--- END TESTS")

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
