## This module implements various helper routines for idioms used throughout
## the compiler. Dependencies on other modules should be kept as minimal as
## possible

type
  HOslice*[T] = object
    ## A half-open slice, that is, a slice where the end is excluded. These
    ## are useful for use with unsigned integers, as, compared to ``Slice``,
    ## no special handling is required for empty slices.
    a*, b*: T

# ----------------- HOslice -----------------

func len*[T](x: HOslice[T]): int {.inline.} =
  ## Returns the number of items in the slice
  int(x.b - x.a)

iterator items*[T](s: HOslice[T]): T =
  ## Returns all items in the slice
  for i in s.a..<s.b:
    yield i
