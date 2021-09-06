"""Create BF code with relative ease.

Various functions are provided that take strings of BF code and return a new
string.
"""

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

def at(delta: int, code: str) -> str:
    """Runs `code` offset by `delta` cells.

    Equivalent to `goto(delta) + code + goto(-delta)`.
    """
    return g(delta) + code + g(-delta)

def loop(code: str) -> str:
    """Runs `code` until the current cell is zero.

    Equivalent to `"[" + code + "]"`.
    """
    return "[" + code + "]"

def loopdown(code: str) -> str:
    """Runs `code` until the current cell is zero while decrementing.

    Equivalent to `loop(change(-1) + code)`.
    """
    return loop(c(-1) + code)

def move(delta: int) -> str:
    """Set cell `delta` away to current cell value and clear current cell.

    Equivalent to `loopdown(at(delta, change(1))`.

    Raises ValueError if delta is 0.
    """
    if delta == 0:
        raise ValueError("target cell cannot be current cell")
    return loopdown(at(delta, c(1)))

def move2(delta: int, delta2: int) -> str:
    """Move current cell value to both cells `delta` and `delta2`.

    Equivalent to `loopdown(at(delta, change(1)) + at(delta2, change(1)))`.

    Raises ValueError if either delta is 0.
    """
    if delta == 0 or delta2 == 0:
        raise ValueError("target cell cannot be current cell")
    return loopdown(at(delta, c(1)) + at(delta2, c(1)))

def copy(temp: int, delta: int) -> str:
    """Copy current cell value to cell `delta` using `temp`.

    Equivalent to `move(temp) + at(temp, move2(-temp + delta, -temp))`.

    Raises ValueError if either delta is 0.
    """
    return move(temp) + at(temp, move2(-temp + delta, -temp))

def init(data: str) -> str:
    """Initializes cells according to `data`.

        >>> init([1, 2])
        "+>++><<"

    """
    return "".join(c(v) + g(1) for v in data) + g(-len(data))

def ifnonzero(then: str) -> str:
    """Runs `then` if the current cell is nonzero.

    Note that the current cell is cleared before the code.
    """
    return loop(setzero() + then)

def ifnonzeroelse(then: str, else_: str, temp: int = 1) -> str:
    """Runs `then` if the current cell is nonzero, otherwise runs `else_`.

    Note that the current cell is cleared before the code.
    """
    return (
        at(temp, setzero() + c(1))
        + ifnonzero(at(temp, c(-1)) + then)
        + at(temp, ifnonzero(at(-temp, else_)))
    )

def switch(default: str, *cases: str, temp: int = 1) -> str:
    """Runs the case matching the current cell value or default otherwise.

    Requires one zeroed temporary cell after the current cell.

    Note that the current cell is cleared after each case.
    """
    if len(cases) == 0:
        return default
    elif len(cases) == 1:
        return ifnonzeroelse(default, cases[0], temp)
    else:
        return ifnonzeroelse(
            c(-1) + switch(*[c(1) + case for case in [default, *cases[1:]]]),
            cases[0],
            temp,
        )
