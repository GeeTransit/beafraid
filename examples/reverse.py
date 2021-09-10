"""Prints BF code that does the equivalent of the following Python code:

print("kid amogus"[::-1]))

"""

from beafraid import *

def ntstart(code=""):
    # Go from after the NT array to before the NT array
    return g(-1) + loop(code + g(-1))

def ntend(code=""):
    # Go from before the NT array to after the NT array
    return g(+1) + loop(code + g(+1))

def ntappend(amount, *, clean=True):
    # Append a new element to the preceding NT array
    return (at(+1, s(0)) if clean else "") + c(amount) + g(+1)

def ntprepend(amount, *, clean=True):
    # Prepend a new element to the succeeding NT array
    return (at(-1, s(0)) if clean else "") + c(amount) + g(-1)

def join(*args):
    return "".join(args)

def reverse(*, clean=True):
    """Reverses the null terminated array starting after the current cell

    In addition to the space the null terminated array takes, this algorithm
    also requires n+2 cells right of the end of the array. If clean is True,
    no assumptions will be made about the current cell nor the memory right of
    the array (meaning they will be set to 0 before used).

    This algorithm works with empty arrays too in case your array might be
    empty.

    """
    # Memory format:
    #   before: 0 a b c 0 ? ? ? ? ?
    #   after : 0 c b a 0 0 0 0 0 0
    return join(
        # Clean up current cell
        s(0) if clean else "",

        # Go to end of NT array
        ntend(),

        # Clean up 2 cells after end of NT array
        # Memory format after: 0 a b c 0 0 0 ? ? ?
        at(1, s(0)) + at(2, s(0)) if clean else "",

        # Loop from last element to first element
        # Memory format after first iteration: 0 a b 0 - 0 c 0 ? ?
        at(-1, loop(join(
            # Subtract 1 from the current element
            c(-1),
            # Append an element to temp NT array with value 1
            at(+1, join(
                ntend() + ntend(),
                ntappend(1, clean=clean),
                ntstart() + ntstart(),
            )),
            # Move rest of current element to temp NT array's last element
            loopdown(at(+1, join(
                ntend() + ntend(),
                at(-1, c(1)),
                ntstart() + ntstart(),
            ))),
            # Prepend a -1 to over NT array
            at(+1, ntprepend(-1, clean=False)),
        ))),
        # Memory format after loop: 0 0 - - - 0 c b a 0

        # Go to start of temp NT array
        ntend(),

        # Loop from first element to last element
        # Memory format after first iteration: 0 c 0 - - - 0 b a 0
        at(+1, loop(join(
            # Subtract 1 from the current element
            c(-1),
            # Append an element to original NT array with value 1 and popleft
            # from the over NT array.
            at(-1, ntstart() + c(1) + g(+1) + c(1) + ntend()),
            # Move rest of current element to original NT array's first element
            loopdown(at(-1, ntstart() + at(-1, c(1)) + ntend())),
            # Append a -1 to the over NT array
            at(-1, ntappend(-1, clean=False)),
        ))),
        # Memory format after first iteration: 0 c b a 0 - - - 0 0

        # Clear over NT array
        # Memory format after: 0 c b a 0 0 0 0 0 0
        ntstart(c(1)),

        # Go to start of original NT array
        ntstart(),
    )

print(
    init([0] + [ord(c) for c in "kid amogus"] + [0])
    + reverse() + ntend(".") + ntstart() + s(10) + "." + s(0)
)
