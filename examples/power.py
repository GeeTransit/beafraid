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

# frame format: break, 0, 0, func, temp, a, b, c
print(
    # push func1(a=6)
    init([0, 0, 0, 1, 0, 6, 0, 0])
    # loop while break!=-1
    + c(1)
    + loop(
        c(-1)
        # check func using temp
        + g(3)
        + switch(
            # default: save func0()
            g(-3) + at(3, setzero()) + g(1),
            # func0(): set break=-1
            g(-3) + c(-1) + g(1),
            # func1(a): save func0() and push func2(a=a)
            g(-3) + at(3, setzero()) + at(8+3, setzero() + c(2)) + at(5, move(-5+8+5)) + g(8+1),
            # func2(a): check a using b
            g(-3) + g(5) + ifnonzeroelse(
                # if a!=0 save func3(a=a) and push func2(a=a-1)
                g(-5) + at(3, setzero() + c(3)) + at(8+3, setzero() + c(2)) + at(5, c(-1) + copy(2, -5+8+5) + c(1)) + g(8+1),
                # else set a=1 and pop
                g(-5) + at(5, setzero() + c(1)) + g(-8+1),
            ),
            # func3(a): save func4(a=a, b=^a) and push func2(a=a-1)
            g(-3) + at(6, setzero()) + at(8+5, move(-5-8+6)) + at(3, setzero() + c(4)) + at(8+3, setzero() + c(2)) + at(5, c(-1) + copy(2, -5+8+5) + c(1)) + g(8+1),
            # func4(a, b): set a=b+^a and pop
            g(-3) + at(7, setzero()) + at(8+5, move(-5-8+7)) + at(5, setzero()) + at(6, move(-6+5)) + at(7, move(-7+5)) + g(-8+1),
        )
        + g(-1)
        + c(1)
    )
    + c(-1)
    # print ^a
    + at(8+5, ".")
)
