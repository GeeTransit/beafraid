import os
import sys

def runbf(program):
    data = bytearray(30000)
    paired_brackets = {}
    unpaired_brackets = []
    inp_buffer = bytearray()
    out_buffer = bytearray()
    i = 0
    j = 0
    if isinstance(program, str):
        program = program.encode()
    while j < len(program):
        char = program[j]
        if char == b"+"[0]:
            if i < 0:
                raise RuntimeError("pointer cannot be negative")
            data[i] = (data[i] + 1) % 256
        elif char == b"-"[0]:
            if i < 0:
                raise RuntimeError("pointer cannot be negative")
            data[i] = (data[i] - 1) % 256
        elif char == b"<"[0]:
            i -= 1
        elif char == b">"[0]:
            i += 1
            if not i < len(data):
                data += b"\x00"
        elif char == b","[0]:
            if i < 0:
                raise RuntimeError("pointer cannot be negative")
            if out_buffer:
                yield out_buffer
                out_buffer.clear()
            if not inp_buffer:
                inp = yield
                if inp is None or inp == -1:
                    inp = b""
                elif isinstance(inp, int):
                    inp_buffer = bytes([inp])
                else:
                    inp_buffer = inp
                inp_buffer = memoryview(inp_buffer)
            if len(inp_buffer) > 0:  # not EOF
                data[i] = inp_buffer[0]
                inp_buffer = inp_buffer[1:]
        elif char == b"."[0]:
            if i < 0:
                raise RuntimeError("pointer cannot be negative")
            out_buffer.append(data[i])
            if len(out_buffer) > 1000:
                yield out_buffer
                out_buffer.clear()
        elif char == b"["[0]:
            if i < 0:
                raise RuntimeError("pointer cannot be negative")
            if j not in paired_brackets:
                # add to stack of unpaired brackets
                unpaired_brackets.append(j)
            if data[i] == 0:
                if j in paired_brackets:
                    j = paired_brackets[j]
                else:
                    # skip to matching ]
                    count = 1
                    j += 1
                    while j < len(program):
                        if program[j] == b"["[0]:
                            unpaired_brackets.append(j)
                            count += 1
                        if program[j] == b"]"[0]:
                            k = unpaired_brackets.pop()
                            paired_brackets[j] = k
                            paired_brackets[k] = j
                            count -= 1
                            if count == 0:
                                break
                        j += 1
                    if j == len(program):
                        j -= 1
        elif char == b"]"[0]:
            if i < 0:
                raise RuntimeError("pointer cannot be negative")
            if j not in paired_brackets:
                if unpaired_brackets:
                    k = unpaired_brackets.pop()
                    paired_brackets[j] = k
                    paired_brackets[k] = j
                else:
                    # ignore unmatched ending brackets
                    paired_brackets[j] = j
            if data[i] == 0:
                pass
            else:
                j = paired_brackets[j]
        j += 1
    if out_buffer:
        yield out_buffer
        out_buffer.clear()

def main(argv):
    if len(argv) < 2:
        print(f"Usage: python runbf.py <filename>", file=sys.stderr)
        return 2

    if argv[1] == "-":
        program = sys.stdin.read()
    else:
        with open(argv[1]) as file:
            program = file.read()

    gen = runbf(program)
    inp = None
    consecutive_eof = 0
    inp_newline_i = 0
    linesep = os.linesep.encode()
    while True:

        try:
            out = gen.send(inp)
        except StopIteration:
            break

        if out is not None:
            if b"\n" in out:
                out = out.replace(b"\n", linesep)
            sys.stdout.buffer.write(out)
        elif consecutive_eof >= 3:
            return 1

        else:
            sys.stdout.flush()
            while True:
                try:
                    inp = sys.stdin.buffer.read(1) or None
                except KeyboardInterrupt:
                    return 1
                if inp is None:
                    consecutive_eof += 1
                else:
                    consecutive_eof = 0
                    if inp[0] == linesep[inp_newline_i]:
                        inp_newline_i += 1
                        if inp_newline_i < len(linesep):
                            continue
                        inp_newline_i = 0
                        inp = b"\n"
                    elif inp_newline_i > 0:
                        inp = linesep[:inp_newline_i] + inp
                break

    return 0

if __name__ == "__main__":
    main(sys.argv)
