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

def cont_switchon(current, delta, *cases, **kwargs):
    cases = [at(-current, case) for case in cases]
    return at(current, switchon(-current + delta, *cases, **kwargs))

# frame format: break, 0, 0, func, temp, a, b, c
print("".join([
    # push func1(a=6)
    init([0, 0, 0, 1, 0, 6, 0, 0]),
    # loop while break!=-1
    loopwhilenot(-1,
        # check func using temp
        cont_switchon(1, 3,
            # default: save func0()
            at(3, s(0)),
            # func0(): set break=-1
            c(-1),
            # func1(a): save func0() and push func2(a=a)
            at(3, s(0)) + at(8+3, s(2)) + movea(5, 8+5) + g(8),
            # func2(a): check a using b
            cont_switchon(1, 5,
                # if a!=0 save func3(a=a) and push func2(a=a-1)
                at(3, s(3)) + at(8+3, s(2)) + at(5, c(-1)) + copya(5, 8+5, 7) + at(5, c(1)) + g(8),
                # else set a=1 and pop
                at(5, s(1)) + g(-8),
            ),
            # func3(a): save func4(a=a, b=^a) and push func2(a=a-1)
            at(6, s(0)) + movea(8+5, 6) + at(3, s(4)) + at(8+3, s(2)) + at(5, c(-1)) + copya(5, 8+5, 7) + at(5, c(1)) + g(8),
            # func4(a, b): set a=b+^a and pop
            at(7, s(0)) + movea(8+5, 7) + at(5, s(0)) + movea(6, 5) + movea(7, 5) + g(-8),
        ),
    ),
    # print ^a
    at(8+5, "."),
]))
