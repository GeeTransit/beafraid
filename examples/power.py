"""Prints BF code that does the equivalent of the following Python code:

def f(x):
    if x == 0:
        return 1
    a = f(x-1)
    b = f(x-1)
    return a + b
print(chr(f(6)), end="")

"""

from beafraid import *

def switchon(delta, *cases, **kwargs):
    cases = [g(-delta) + case for case in cases]
    return g(delta) + switch(*cases, **kwargs)

def movea(origin, target):
    return at(origin, move(-origin + target))

def copya(origin, target, temp):
    return at(origin, copy(-origin + temp, -origin + target))

def loopwhilenot(amount, code):
    return c(-amount) + loop(c(amount) + code + c(-amount)) + c(amount)

CONT_FRAMESIZE = 8
[
    CONT_BREAK, CONT_ZERO, CONT_ZERO_TEMP,
    CONT_FUNC, CONT_FUNC_TEMP,
    CONT_A, CONT_B, CONT_C,
] = range(CONT_FRAMESIZE)
CONT_BREAKON = -1

def cont_switchon(delta, *cases, **kwargs):
    cases = [at(-CONT_ZERO, case) for case in cases]
    return at(CONT_ZERO, switchon(-CONT_ZERO + delta, *cases, **kwargs))

def cont_save(func):
    return at(CONT_FUNC, s(func))

def cont_push(func):
    return g(CONT_FRAMESIZE) + cont_save(func)

def cont_pop():
    return g(-CONT_FRAMESIZE)

def cont_loop(code):
    return at(CONT_BREAK, loopwhilenot(CONT_BREAKON, code))

def cont_break():
    return at(CONT_BREAK, s(CONT_BREAKON))

# frame format: break, 0, 0, func, temp, a, b, c
print("".join([
    # push func1(a=6)
    init([0, 0, 0, 1, 0, 6, 0, 0]),
    # loop while break!=-1
    cont_loop(
        # check func using temp
        cont_switchon(CONT_FUNC,
            # default: save func0()
            cont_save(0),
            # func0(): set break=-1
            cont_break(),
            # func1(a): save func0() and push func2(a=a)
            movea(CONT_A, CONT_FRAMESIZE+CONT_A) + cont_save(0) + cont_push(2),
            # func2(a): check a using b
            cont_switchon(CONT_A,
                # if a!=0 save func3(a=a) and push func2(a=a-1)
                at(CONT_A, c(-1)) + copya(CONT_A, CONT_FRAMESIZE+CONT_A, CONT_C) + at(CONT_A, c(1)) + cont_save(3) + cont_push(2),
                # else set a=1 and pop
                at(CONT_A, s(1)) + cont_pop(),
            ),
            # func3(a): save func4(a=a, b=^a) and push func2(a=a-1)
            movea(CONT_FRAMESIZE+CONT_A, CONT_B) + at(CONT_A, c(-1)) + copya(CONT_A, CONT_FRAMESIZE+CONT_A, CONT_C) + at(CONT_A, c(1)) + cont_save(4) + cont_push(2),
            # func4(a, b): set a=b+^a and pop
            at(CONT_C, s(0)) + movea(CONT_FRAMESIZE+CONT_A, CONT_C) + at(CONT_A, s(0)) + movea(CONT_B, CONT_A) + movea(CONT_C, CONT_A) + cont_pop(),
        ),
    ),
    # print ^a
    at(CONT_FRAMESIZE+CONT_A, "."),
]))
