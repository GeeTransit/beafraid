import sys

if len(sys.argv) < 2:
    sys.exit(f"Usage: python runbf.py <filename>")

with open(sys.argv[1]) as file:
    program = file.read()

data = bytearray(30000)
paired_brackets = {}
unpaired_brackets = []

i = 0
j = 0
while j < len(program):
    char = program[j]
    if char == "+":
        data[i] = (data[i] + 1) % 256
    elif char == "-":
        data[i] = (data[i] - 1) % 256
    elif char == "<":
        if i > 0:
            i -= 1
    elif char == ">":
        i += 1
        if not i < len(data):
            data += b"\x00"
    elif char == ",":
        sys.stdout.flush()
        inp = sys.stdin.read(1)
        data[i] = ord(inp) if inp else 255
    elif char == ".":
        print(chr(data[i]), end="")
        # print(data[i])
    elif char == "[":
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
                    if program[j] == "[":
                        unpaired_brackets.append(j)
                        count += 1
                    if program[j] == "]":
                        k = unpaired_brackets.pop()
                        paired_brackets[j] = k
                        paired_brackets[k] = j
                        count -= 1
                        if count == 0:
                            break
                    j += 1
                if j == len(program):
                    j -= 1
    elif char == "]":
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
