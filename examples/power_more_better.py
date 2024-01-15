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
from .s4op import pack_class_offsets, size, Struct, Offset

def join(*parts):
    return "\n".join(parts)

def switchon(delta, *cases, **kwargs):
    cases = [g(-delta) + case for case in cases]
    return g(delta) + switch(*cases, **kwargs)

def movea(origin, target):
    return at(origin, move(-origin + target))

def copya(origin, target, temp):
    return at(origin, copy(-origin + temp, -origin + target))

def loopwhilenot(amount, code):
    return c(-amount) + loop(c(amount) + code + c(-amount)) + c(amount)

@pack_class_offsets
class Cont(Struct):
    """A continuation structure with a break cell and a landing zone"""
    break_ = Offset(0)
    land = Offset(range(2))

@pack_class_offsets
class Data(Struct):
    """A structure with data cells"""
    a = Offset()
    b = Offset()
    c = Offset()

@pack_class_offsets
class Frame(Struct):
    """A frame structure with Cont head, a function switch, and data cells"""
    cont = Offset(Cont())
    func = Offset(range(2))
    data = Offset(Data())

    BREAKON = -1

    def switchon(self, deltas, *cases, **kwargs):
        # The pointer must be at some other jump point because the temporary cell
        # is zeroed before each case.
        if size(deltas) != 2:
            raise ValueError("deltas must have length 2")
        if deltas[0] + 1 != deltas[1]:
            raise ValueError("deltas aren't adjacent")
        cases = [
            at(-self.cont.land[0], at(deltas[1], s(0)) + case)
            for case in cases
        ]
        return at(
            self.cont.land[0],
            switchon(-self.cont.land[0] + deltas[0], *cases, **kwargs),
        )

    def save(self, func=None):
        return at(self.func[0], s(func) if func is not None else "")

    def push(self, func=None):
        return g(len(self)) + self.save(func)

    def pop(self, func=0):
        return self.save(func) + g(-len(self))

    def loop(self, code):
        return at(
            self.cont.break_,
            loopwhilenot(self.BREAKON, at(-self.cont.break_, code)),
        )

    def break_(self):
        return at(self.cont.break_, s(self.BREAKON)) + self.save(0)

frame = Frame()
data = frame.data

print(join(
    # push func1(a=6)
    at(data.a, s(6)),
    frame.save(1),
    # loop while break!=-1
    frame.loop(
        # check func using temp
        frame.switchon(frame.func,
            # default: save func0()
            frame.save(0),
            # func0(): set break=-1
            frame.break_(),
            # func1(a): save func0() and push func2(a=a)
            join(
                movea(data.a, len(frame)+data.a),
                frame.save(0),
                frame.push(2),
            ),
            # func2(a): check a using b
            frame.switchon((data.a, data.b),
                # if a!=0 save func3(a=a) and push func2(a=a-1)
                join(
                    at(data.a, c(-1)),
                    copya(data.a, len(frame)+data.a, data.c),
                    at(data.a, c(1)),
                    frame.save(3),
                    frame.push(2),
                ),
                # else set a=1 and pop
                join(
                    at(data.a, s(1)),
                    frame.pop(),
                ),
            ),
            # func3(a): save func4(a=a, b=^a) and push func2(a=a-1)
            join(
                movea(len(frame)+data.a, data.b),
                at(data.a, c(-1)),
                copya(data.a, len(frame)+data.a, data.c),
                at(data.a, c(1)),
                frame.save(4),
                frame.push(2),
            ),
            # func4(a, b): set a=b+^a and pop
            join(
                at(data.c, s(0)),
                movea(len(frame)+data.a, data.c),
                at(data.a, s(0)),
                movea(data.b, data.a),
                movea(data.c, data.a),
                frame.pop(),
            ),
        ),
    ),
    # print ^a
    at(len(frame)+data.a, "."),
))
