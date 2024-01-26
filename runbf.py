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

    # Convert program into intermediate representation
    instructions = []
    offset = 0
    def optimize(instructions):
        # return
        if not instructions:
            return [["ifnonzero", [["error"]]]]
        if instructions[-1][0] == ">":
            return None  # We do not optimize unbalanced loops currently
        if all(instr[0] == "+" for instr in instructions):
            changes = {}
            for instr in instructions:
                if instr[1] not in changes:
                    changes[instr[1]] = 0
                changes[instr[1]] += instr[2]
                if changes[instr[1]] % 256 == 0:
                    del changes[instr[1]]
            if changes.get(0, 0) % 256 == -1 % 256:
                del changes[0]
                if changes:
                    return [["ifnonzero", [["movemul", [[key, value] for key, value in changes.items()]]]]]
                else:
                    return [["clear"]]
            elif changes.get(0, 0) % 256 == 1:
                del changes[0]
                if changes:
                    return [["ifnonzero", [["movemul", [[key, -value] for key, value in changes.items()]]]]]
                else:
                    return [["clear"]]
            else:
                x = changes.pop(0, 0) % 256
                if x != 0:
                    return [["ifnonzero", [["movemulx", x, [[key, value] for key, value in changes.items()]]]]]
                else:
                    return [["ifnonzero", [["error"]]]]
        return None
    for char in program:
        if char == b"+"[0] or char == b"-"[0]:
            instructions.append(["+", offset, 1 if char == b"+"[0] else -1])
            if len(instructions) >= 2 and instructions[-2][0] == "+" and instructions[-2][1] == offset:
                instructions[-2][2] += instructions.pop()[2]
                if instructions[-1][2] % 256 == 0:
                    instructions.pop()
        elif char == b"<"[0]:
            offset -= 1
        elif char == b">"[0]:
            offset += 1
        elif char == b","[0]:
            instructions.append([",", offset])
        elif char == b"."[0]:
            instructions.append([".", offset])
        elif char == b"["[0]:
            if offset != 0:
                instructions.append([">", offset])
                offset = 0
            instructions = [["preloop", instructions]]
        elif char == b"]"[0]:
            if offset != 0:
                instructions.append([">", offset])
                offset = 0
            if not instructions or instructions[0][0] != "preloop":
                instructions = [["dowhile", instructions]]
            else:
                preloop = instructions.pop(0)[1]
                preloop.extend(optimize(instructions) or [["loop", instructions]])
                instructions = preloop
    if offset != 0:
        instructions.append([">", offset])
        offset = 0
    while instructions and instructions[0][0] == "preloop":
        preloop = instructions.pop(0)[1]
        preloop.append(["ifnonzero", instructions])
        instructions = preloop

    # Interpret the intermediate representation
    stack = [[0, instructions]]
    while stack:
        j, instructions = stack[-1]
        for j in range(j, len(instructions)):
            instr = instructions[j]

            if instr[0] == "+":
                if i + instr[1] < 0:
                    raise RuntimeError("pointer cannot be negative")
                while i + instr[1] >= len(data):
                    data += b"\x00"

                data[i + instr[1]] = (data[i + instr[1]] + instr[2]) % 256

            elif instr[0] == ">":
                i += instr[1]

            elif instr[0] == ",":
                if i + instr[1] < 0:
                    raise RuntimeError("pointer cannot be negative")
                while i + instr[1] >= len(data):
                    data += b"\x00"

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
                    data[i + instr[1]] = inp_buffer[0]
                    inp_buffer = inp_buffer[1:]

            elif instr[0] == ".":
                if i + instr[1] < 0:
                    raise RuntimeError("pointer cannot be negative")
                while i + instr[1] >= len(data):
                    data += b"\x00"

                out_buffer.append(data[i + instr[1]])
                if len(out_buffer) > 1000:
                    yield out_buffer
                    out_buffer.clear()

            elif instr[0] == "ifnonzero":
                stack[-1][0] = j + 1
                if data[i] != 0:
                    stack.append([0, instr[1]])
                    break

            elif instr[0] == "loop":
                if data[i] != 0:
                    stack[-1][0] = j
                    stack.append([0, instr[1]])
                    break

            elif instr[0] == "dowhile":
                raise NotImplementedError

            elif instr[0] == "error":
                raise RuntimeError("error")

            elif instr[0] == "movemul":
                x = data[i]
                for key, value in instr[1]:
                    if i + key < 0:
                        raise RuntimeError("pointer cannot be negative")
                    while i + key >= len(data):
                        data += b"\x00"
                    data[i + key] = (data[i + key] + x*value) % 256
                data[i] = 0

            elif instr[0] == "movemulx":
                x = data[i]
                for xx in range(256):
                    if (x + xx*instr[1]) % 256 == 0:
                        break
                else:
                    raise RuntimeError(f'cell not multiple of {instr[1]}')
                for key, value in instr[2]:
                    if i + key < 0:
                        raise RuntimeError("pointer cannot be negative")
                    while i + key >= len(data):
                        data += b"\x00"
                    data[i + key] = (data[i + key] + xx*value) % 256
                data[i] = 0

            elif instr[0] == "clear":
                data[i] = 0

        else:
            stack.pop()

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
