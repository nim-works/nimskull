discard """
  action: run
  target: c
"""

block:
  type Complex = object
    # using a GC'ed type requires a complex assignment
    a: ref int

  proc p() =
    let x = Complex(a: new int)
    doAssert x.a[] == 0

  p()