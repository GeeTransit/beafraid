"""Offset with ease

s4op provides utility functions operating on offsets along with class based
utilities paralleling structs found in C.

To create a struct with 3 offsets, you can write the following:

    >>> @pack_class_offsets
    ... class Thing(Struct):
    ...     a = Offset()
    ...     b = Offset()
    ...     c = Offset()
    ...

The pack_class_offsets decorator "packs" the offsets to be beside each other.
You can tell by creating a Thing object and checking its offsets:

    >>> t = Thing()
    >>> t.a, t.b, t.c
    (0, 1, 2)
    >>> u = Thing(5)
    >>> u.a, u.b, u.c
    (5, 6, 7)

How does s4op know the starting position of a struct? Each struct has an offset
property that tells us the struct of the entire struct. You can access it
directly or use the start function:

    >>> t.offset, start(t)
    (0, 0)
    >>> u.offset, start(u)
    (5, 5)

How does s4op know the size of a struct? Well, it uses an offset called end to
mark the end of a struct. The pack_class_offsets decorator adds it for us
however so we don't need to. You can get it directly or use the size function:

    >>> t.end, size(t)
    (3, 3)
    >>> u.end, size(u)
    (8, 3)

To help with applying offsets, s4op has a shift function that works with many
different types of offsets. This is what structs and offsets use internally:

    >>> shift(Thing(), 5)
    Thing(5)
    >>> shift(Thing().a, 5)
    5
    >>> shift(0, 5)
    5
    >>> shift(range(2), 5)
    range(5, 7)

To pack offsets, s4op has a pack function (along with pack_inplace and the
pack_class_offsets decorator) that shift offsets depending on the size of the
offsets before it:

    >>> pack(Offset(0), Offset(range(2)))
    (Offset(0), Offset(range(1, 3)))
    >>> pack(Offset(Thing()), Offset(Thing()))
    (Offset(Thing(0)), Offset(Thing(3)))

"""

__version__ = "0.1"

import copy

def start(offset):
    """Returns the start position of an offset

        >>> start(0)
        0
        >>> start(range(2))
        0
        >>> start((0, 1, 2))
        0
        >>> start(Offset(0))
        0
        >>> start(Offset(0, 5))
        5

    """
    if isinstance(offset, int):
        return offset
    if isinstance(offset, range):
        return offset.start
    if isinstance(offset, Offset):
        return start(offset(0))
    if isinstance(offset, Struct):
        return offset.offset
    if isinstance(offset, (tuple, list)):
        return start(offset[0])
    raise TypeError(f"cannot pass a {type(offset).__name__} object to start")

def size(offset):
    """Returns the size of an offset

    For structs, its end is returned.

        >>> size(0)
        1
        >>> size(range(2))
        2
        >>> size((0, 1, 2))
        3
        >>> size(Offset(0))
        1
        >>> class Things(Struct):
        ...     a = Offset(0)
        ...     b = Offset(1)
        ...     end = Offset(2)
        ...
        >>> size(Things())
        2
        >>> size(Things(5))
        2

    """
    if isinstance(offset, int):
        return 1
    if isinstance(offset, range):
        return len(offset)
    if isinstance(offset, (Offset, Struct)):
        return len(offset)
    if isinstance(offset, (tuple, list)):
        return sum(size(part) for part in offset)
    raise TypeError(f"cannot pass a {type(offset).__name__} object to size")

def shift(offset, by: int):
    """Returns a copy of offset with each value shifted

        >>> shift(0, 5)
        5
        >>> shift(range(2), 5)
        range(5, 7)
        >>> shift((0, 1, 2), 5)
        (5, 6, 7)
        >>> shift(Offset(0), 5)
        Offset(5)

    """
    if isinstance(offset, int):
        return offset + by
    if isinstance(offset, range):
        return range(
            offset.start + by,
            offset.stop + by,
            offset.step,
        )
    if isinstance(offset, (tuple, list)):
        return tuple(shift(part, by) for part in offset)
    if isinstance(offset, Offset):
        obj = copy.copy(offset)
        obj.by += by
        return obj
    if isinstance(offset, Struct):
        obj = copy.copy(offset)
        obj.offset += by
        return obj
    raise TypeError(f"cannot pass a {type(offset).__name__} object to shift")

class Offset:
    """A descriptor that shifts by its owner's offset when accessed

    Uses the shift function to apply the offset.

        >>> o = Offset(range(2), 2)
        >>> o.offset, o.by
        (range(0, 2), 2)
        >>> o(5)
        range(7, 9)
        >>> class Things:
        ...     a = Offset(range(0, 2))
        ...     b = Offset(range(2, 4))
        ...     def __init__(self, offset):
        ...         self.offset = offset
        ...
        >>> t = Things(3)
        >>> t.a
        range(3, 5)
        >>> t.b
        range(5, 7)

    """
    __match_args__ = ("offset", "by")

    def __init__(self, offset = 0, by: int = 0):
        self.offset = offset
        self.by = by

    def __repr__(self):
        return f"{type(self).__name__}({shift(self.offset, self.by)!r})"

    def __get__(self, obj, cls=None):
        if obj is None:
            return self
        return self(getattr(obj, "offset", 0))

    def __set__(self, obj, value):
        raise AttributeError

    def __copy__(self):
        return type(self)(copy.copy(self.offset), self.by)

    def __len__(self):
        return size(self.offset)

    def __call__(self, offset):  # intentionally missing a default value
        return shift(shift(self.offset, self.by), offset)

class Struct:
    """A base class that wraps a collection of offsets

        >>> class Things(Struct):
        ...     a = Offset(0)
        ...     b = Offset(1)
        ...     end = Offset(2)
        ...
        >>> Things().a, Things().b
        (0, 1)
        >>> Things(5).a, Things(5).b
        (5, 6)

    """
    __match_args__ = ("offset",)

    def __init__(self, offset: int = 0):
        self.offset = offset

    def __repr__(self):
        return f"{type(self).__name__}({self.offset!r})"

    def __copy__(self):
        return type(self)(self.offset)

    def __len__(self):
        return start(type(self).end)

    def __getitem__(self, value):
        return range(self.offset, self.offset + len(self))[value]

def pack_inplace(*offsets: Offset):
    """Modifies the offsets to be packed consecutively

        >>> a, b = Offset(0), Offset((0, 1, 2))
        >>> pack_inplace(a, b)
        >>> a, b
        (Offset(0), Offset((1, 2, 3)))

    """
    start = 0
    last = ()
    for offset in offsets:
        start += size(last)
        offset.by += start
        last = offset

def pack(*offsets: Offset):
    """Returns a copy of the offsets that are packed consecutively

        >>> pack(Offset(0), Offset((0, 1, 2)))
        (Offset(0), Offset((1, 2, 3)))

    """
    by = 0
    last = ()
    result = []
    for offset in offsets:
        by += size(last)
        result.append(shift(offset, by))
        last = offset
    return tuple(result)

def pack_class_offsets(cls: type = None, *, end: bool = True):
    """Pack offsets of cls with an end offset appended by default

    Uses the pack_inplace function to pack the offsets.

    Pass end=False to disable appending an end offset

        >>> @pack_class_offsets
        ... class Things(Struct):
        ...     a = Offset(0)
        ...     b = Offset((0, 1, 2))
        ...
        >>> Things().a
        0
        >>> Things().b
        (1, 2, 3)
        >>> Things().end
        4
        >>> @pack_class_offsets(end=False)
        ... class ThingsNoEnd(Struct):
        ...     a = Offset(0)
        ...     b = Offset((0, 1, 2))
        ...
        >>> hasattr(ThingsNoEnd(), "end")
        False

    """
    append_end_offset = end

    def _pack_class(cls: type):
        offsets = []
        for name, offset in vars(cls).items():
            if isinstance(offset, Offset):
                offsets.append(offset)

        if append_end_offset:
            end = Offset()
            offsets.append(end)
            cls.end = end
            getattr(end, "__set_name__", lambda *_: None)(cls, "end")

        pack_inplace(*offsets)
        return cls

    if cls is None:
        return _pack_class
    else:
        return _pack_class(cls)
