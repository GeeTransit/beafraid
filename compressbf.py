"""Compress BF code."""

import sys
from collections import defaultdict

from beafraid import goto, change

# - Action constants

NORMAL = "normal"
KEEP = "keep"
REMOVE = "remove"

# - Various compression functions

def remove_impossible_loops(
    code: str,
    *,
    unmatched_end_brackets: str = NORMAL,
    unknown_characters: str = KEEP,
    initial_loop: str = REMOVE,
) -> str:
    """Remove header loops or loops directly after other loops.

    If unmatched_end_brackets is NORMAL, unmatched end brackets will be kept in
    the output and subsequent loops will be removed. If KEEP, they will be kept
    in the output but otherwise ignored. If REMOVE, they will be removed from
    the output.

    If unknown_characters is KEEP, unknown characters will be kept in the
    output. If REMOVE, they will be removed from the output,

    If initial_loop is REMOVE, the first loop will be removed as if there was
    a loop before it. If KEEP, the first loop will be kept.

    """
    if unmatched_end_brackets not in [NORMAL, KEEP, REMOVE]:
        raise ValueError(f"invalid action: {unmatched_end_brackets!r}")
    if unknown_characters not in [KEEP, REMOVE]:
        raise ValueError(f"invalid action: {unknown_characters!r}")
    if initial_loop not in [KEEP, REMOVE]:
        raise ValueError(f"invalid action: {initial_loop!r}")

    # Initialize state
    result = []  # List of characters to be joined and returned
    depth = 0  # Used to identify unmatched end brackets
    can_skip = True  # Only true initially and after other loops
    skip_depth = 0  # Number of brackets deep or 0 when not skipping

    # If we're keeping the initial loop, clear can_skip
    if initial_loop == KEEP:
        can_skip = False

    # Loop over code's characters
    for char in code:

        # If we're currently skipping
        if skip_depth != 0:
            # Remove all characters. If skip_depth becomes 0, the next
            # loop iteration will handle that accordingly.
            if char == "[":
                depth += 1
                skip_depth += 1
            elif char == "]":
                depth -= 1
                skip_depth -= 1
                assert can_skip
            continue

        # If the operation might change the current cell value or location,
        # subsequent loops can't be removed safely.
        if char in "+-<>,":
            can_skip = False

        # If the operation won't change the current cell value or location,
        # ignore it.
        elif char in ".":
            pass

        # On start brackets...
        elif char == "[":
            depth += 1
            # If this loop can be skipped, start skipping
            if can_skip:
                skip_depth = 1
                # Remove this character
                continue

        # On end brackets...
        elif char == "]":
            # If this bracket is matched, set the can_skip flag
            if depth != 0:
                depth -= 1
                can_skip = True
            # Otherwise, act according to unmatched_end_brackets
            else:
                if unmatched_end_brackets == NORMAL:
                    # Skip the next loop
                    can_skip = True
                elif unmatched_end_brackets == KEEP:
                    # Keep in output but otherwise ignore
                    pass
                elif unmatched_end_brackets == REMOVE:
                    # Remove this character
                    continue
                else:
                    assert False

        # On unknown characters, act according to unknown_characters
        else:
            if unknown_characters == KEEP:
                # Keep in output but otherwise ignore
                pass
            elif unknown_characters == REMOVE:
                # Remove this character
                continue
            else:
                assert False

        # Add this character to the result
        result.append(char)

    # Return the cleaned code
    return "".join(result)

def simplify_constant_operations(code: str) -> str:
    """Simplify strings of "+-<>" operations

    Unknown characters between those operations will be removed in the output.

    This is similar to constant folding.

    """
    # Initialize state
    result = []  # List of characters to be joined and returned
    updates = defaultdict(int)  # Map from relative positions to their change
    pointer = 0  # The pointer's current relative position

    # Return whether or not there are updates
    def _check_updates():
        return pointer != 0 or any(value != 0 for value in updates.values())

    # Processes updates and adds the code to result
    def _process_updates():
        # Get positions that need to be updated
        order = sorted(pos for pos, value in updates.items() if value != 0)

        # Updates might be empty (such as a string of ">"s)
        if order:

            # If traversing the positions backwards generates less
            # operations than traversing forwards, reverse the order.
            distance_normal = abs(min(order)) + abs(pointer - max(order))
            distance_reverse = abs(max(order)) + abs(pointer - min(order))
            if distance_reverse < distance_normal:
                order.reverse()

        # Generate operations for the specified updates
        current = 0
        for pos in order:
            result.append(goto(pos - current))
            result.append(change(updates[pos]))
            current = pos

        # Go to the ending position after finishing traversal
        result.append(goto(pointer - current))

    # Loop over code's characters
    for char in code:

        # If the operation is simplifiable
        if char in "+-<>":
            if char == "+":
                updates[pointer] += 1
            elif char == "-":
                updates[pointer] -= 1
            elif char == "<":
                pointer -= 1
            elif char == ">":
                pointer += 1
            else:
                assert False
            continue

        # If the operation isn't simplifiable
        elif char in ",.[]#":
            # If there are updates to be processed, process, generate, and
            # insert the code before this operation.
            if _check_updates():
                _process_updates()
                # Reset state
                pointer = 0
                updates.clear()

        # On unknown characters...
        else:
            # If there are updates to be processed, remove them
            if _check_updates():
                continue
            # Otherwise, keep them
            else:
                pass

        # Add this character to the result
        result.append(char)

    # If there are updates to be processed, process them
    if _check_updates():
        _process_updates()

    # Return the cleaned code
    return "".join(result)

def compress(code: str) -> str:
    """Compress and return the compressed code.

    A variety of basic compression strategies are used:

    - remove_impossible_loops
    - simplify_constant_operations

    """
    # Remove loops that follow other loops
    for _ in range(20):  # Don't let it get out of control
        old_len = len(code)
        code = remove_impossible_loops(code)
        code = simplify_constant_operations(code)
        if len(code) == old_len:
            break
    return code

# - Test functions

def test_remove_impossible_loops():
    assert remove_impossible_loops("[a]") == ""
    assert remove_impossible_loops("+[a]") == "+[a]"
    assert remove_impossible_loops("+[a][a]") == "+[a]"
    assert remove_impossible_loops("+[a+][a+]") == "+[a+]"
    assert remove_impossible_loops("+[a+]+[a+]") == "+[a+]+[a+]"

    assert remove_impossible_loops("+[a+[a+]a[a]+]+[a+]") == "+[a+[a+]a+]+[a+]"

def test_remove_impossible_loops_dont_ignore_unmatched_end_brackets():
    assert remove_impossible_loops("]", ignore_unmatched_end_brackets=False) == "]"
    assert remove_impossible_loops("][", ignore_unmatched_end_brackets=False) == "]"
    assert remove_impossible_loops("]a[", ignore_unmatched_end_brackets=False) == "]a"
    assert remove_impossible_loops("]+[", ignore_unmatched_end_brackets=False) == "]+["

    assert remove_impossible_loops("+[+]+][+]", ignore_unmatched_end_brackets=False) == "+[+]+]"

def test_remove_impossible_loops_ignore_unmatched_end_brackets():
    assert remove_impossible_loops("]", ignore_unmatched_end_brackets=True) == "]"
    assert remove_impossible_loops("][", ignore_unmatched_end_brackets=True) == "]"
    assert remove_impossible_loops("]a[", ignore_unmatched_end_brackets=True) == "]a"
    assert remove_impossible_loops("]+[", ignore_unmatched_end_brackets=True) == "]+["

    assert remove_impossible_loops("+[+]+][+]", ignore_unmatched_end_brackets=False) == "+[+]+][+]"

# - Command line

def main(argv):
    if len(argv) < 2:
        print(f"Usage: python compressbf.py <filename>", file=sys.stderr)
        return 2
    if argv[1] == "-":
        program = sys.stdin.read()
    else:
        with open(argv[1]) as file:
            program = file.read()
    print(compress(program), end="")

if __name__ == "__main__":
    main(sys.argv)
