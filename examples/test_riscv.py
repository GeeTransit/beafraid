from .riscv import *

_DEBUG_ = False

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
    def dump(self, data, length=80, func=print):
        convert = self.convert
        for i in range(len(data))[::length]:
            func(convert(data[i:i+length]))
Icp437.i = Icp437()
icp437 = Icp437.i.convert


_INSTR = lambda n: "error" if n == 255 else "load mathi store math branch env auipc lui jalr jal fence".split()[n-1]
def test_DECODE_OPCODE_TYPE():
    func = DECODE_OPCODE_TYPE
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
            assert "IISRBRUUIJI"[n-1] == "RISBUJ"[f], n
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

def test_DECODE_INSTR_PARTS():
    func = DECODE_INSTR_PARTS
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

def whenthe3():
    func = EXECUTE
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

def test_MAINLOOP():
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

# Used https://riscemu.readthedocs.io/en/latest/riscemu.html to match output
def riscv_test(_check=None, *, _run=False, name=None, _hex=None, memory=b"", registers=b"", pc=b"", _rng=None, _debug=False, _code=[]):
    if _check is None:
        return lambda _check, *, _kwargs=locals(), **kwargs: riscv_test(**{**_kwargs, **kwargs, "_check": _check})

    if not _run:
        return lambda *, _kwargs={**locals(), "_run": True}: riscv_test(**_kwargs)

    if name is None:
        name = _check.__name__

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
    # if data is not None:
        # assert seed is None, "seed not supported with hypothesis"
        # rng = data.draw(randoms())
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
        # print("passed")

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
    assert outmem[h.N] == E_FENCE % 256
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
    assert outmem[h.N] == E_SYSTEM % 256
    assert outmem[h.x2] == outmem[HEAD+REGS:][8]
