"""Create BF code with relative ease.

Various functions are provided that take strings of BF code and return a new
string.
"""

import warnings

def goto(delta: int) -> str:
    """Move the cell pointer by `delta` cells where left is positive.

    If `delta` is positive, the result is equivalent to `">" * delta`. If
    `delta` is negative, the result is equivalent to `"<" * -delta`.
    """
    if delta >= 0:
        return ">" * delta
    else:
        return "<" * -delta
g = goto

def change(amount: int) -> str:
    """Change the current cell by `amount`.

    If `amount` is positive, the result is equivalent to `"+" * amount`. If
    `amount` is negative, the result is equivalent to `"-" * -amount`.
    """
    if amount >= 0:
        return "+" * amount
    else:
        return "-" * -amount
c = change

def setzero() -> str:
    """Sets the current cell to 0.

    Equivalent to `"[-]"`.
    """
    return "[-]"

# Return a single number
def _one(nums) -> int:
    if isinstance(nums, int):
        return nums
    if len(nums) == 1:
        return list(nums)[0]
    raise TypeError("could not get an offset")

# Return a list of numbers
def _many(nums) -> list:
    if isinstance(nums, int):
        return [nums]
    if isinstance(nums, list):
        return nums
    return list(nums)

def at(delta: int, code: str) -> str:
    """Runs `code` offset by `delta` cells.

    Equivalent to `goto(delta) + code + goto(-delta)`.
    """
    return "".join(g(d) + code + g(-d) for d in _many(delta))

def assign(amount: int) -> str:
    """Set the current cell value to `amount`.

    Equivalent to `setzero() + change(amount)`.
    """
    return setzero() + c(amount)
s = assign

def loop(a, b=None) -> str:
    """loop(code) -> run code until current cell is zero
    loop(delta, code) -> run code until delta cell is zero

    Runs `code` until the current/delta cell is zero.

    Equivalent to `"[" + code + "]"`.
    """
    if b is None:
        a, b = 0, a
    delta, code = a, b
    delta = _many(delta)
    return "".join(at(d, "[") + code + at(d, "]") for d in delta)

def loopdown(a, b=None) -> str:
    """loopdown(code) -> run code until current cell is zero with decrement
    loopdown(delta, code) -> run code until delta cell is zero with decrement

    Runs `code` until the current/delta cell is zero while decrementing.

    Equivalent to `loop(change(-1) + code)`.
    """
    if b is None:
        a, b = 0, a
    delta, code = a, b
    delta = _many(delta)
    return "".join(loop(d, at(d, c(-1)) + code) for d in delta)

def loopuntil(amount, a, b=None):
    """loopuntil(amount, code) -> run code until current cell equals amount
    loopuntil(amount, delta, code) -> run code until delta cell equals amount

    Runs `code` until the current/delta cell is equal to `amount`.
    """
    if b is None:
        a, b = 0, a
    delta, code = a, b
    return (
        at(delta, c(-amount))
        + loop(delta, at(delta, c(amount)) + code + at(delta, c(-amount)))
        + at(delta, c(amount))
    )

def move(a, b=None) -> str:
    """move(target) -> move current to target
    move(origin, target) -> move origin to target

    Set cell `target` away to current/origin cell value and clear
    current/origin cell.

    Equivalent to `at(origin, loopdown(at(-origin + delta, change(1))))`.

    Raises ValueError if target is 0.
    """
    if b is None:
        a, b = 0, a
    origin, delta = a, b
    origin = _many(origin)
    delta = _many(delta)
    if any(o in delta for o in origin):
        raise ValueError("target cell cannot be current cell")
    return loopdown(origin, at(delta, c(1)))

def move2(delta: int, delta2: int) -> str:
    """Move current cell value to both cells `delta` and `delta2`.

    Equivalent to `loopdown(at(delta, change(1)) + at(delta2, change(1)))`.

    Raises ValueError if either delta is 0.
    """
    warnings.warn("use move([delta, delta2]) instead", DeprecationWarning, 2)
    if delta == 0 or delta2 == 0:
        raise ValueError("target cell cannot be current cell")
    return loopdown(at(delta, c(1)) + at(delta2, c(1)))

def copy(temp: int, delta: int) -> str:
    """Copy current cell value to cell `delta` using `temp`.

    Equivalent to `move(temp) + at(temp, move2(-temp + delta, -temp))`.

    Raises ValueError if either delta is 0.
    """
    temp = _one(temp)
    delta = _many(delta)
    if temp in delta:
        raise ValueError("target cell cannot be temporary cell")
    return move(temp) + loopdown(temp, at([0, *delta], c(1)))

def init(data: list) -> str:
    """Initializes cells according to `data`.

        >>> init([1, 2])
        "+>++><<"

    """
    return "".join(c(v) + g(1) for v in data) + g(-len(data))

def ifnonzero(a, b=None) -> str:
    """ifnonzero(then) -> run then if current cell is nonzero
    ifnonzero(delta, then) -> run then if delta cell is nonzero

    Runs `then` if the current/delta cell is nonzero.

    Note that the current/delta cell is cleared after the code.
    """
    if b is None:
        a, b = 0, a
    delta, then = a, b
    delta = _one(delta)
    return loop(delta, then + at(delta, s(0)))

def ifnonzeroelse(a, b, d=None, *, temp=None) -> str:
    """ifnonzeroelse(then, else_, temp=1) -> if current nonzero then, else
    ifnonzeroelse(delta, then, else_, temp=1) -> if delta nonzero then, else

    Runs `then` if the current/delta cell is nonzero, otherwise runs `else_`.

    Note that the current/delta cell is cleared after the code.
    """
    if d is None:
        a, b, d = 0, a, b
    delta, then, else_ = a, b, d
    delta = _one(delta)
    temp = _one(temp)
    return (
        at(temp, c(1))
        + ifnonzero(delta, at(temp, c(-1)) + then)
        + ifnonzero(temp, at(temp, c(-1)) + else_)
    )

def switch(default: str = None, *cases: str, temp: int = None, on: int = 0) -> str:
    """Runs the case matching the current/on cell value or default otherwise.

    Requires one zeroed temporary cell after the current/on cell.

    Note that the current/on cell is cleared after each case.
    """
    if default is None:
        return lambda default, *cases, temp=temp, on=on: switch(default, *cases, temp=temp, on=on)
    if on is None:
        on = 0
    if temp is None:
        temp = on + 1
    cases = list(cases)
    for i, case in enumerate(cases):
        if not isinstance(case, list):
            cases[i] = [cases[i-1][0] + 1 if i > 0 else 0, case]
    code = default
    for x, case in reversed(cases):
        code = at(on, c(-x)) + ifnonzeroelse(on, at(on, c(x)) + code, at(on, c(x)) + case, temp=temp)
    return code
