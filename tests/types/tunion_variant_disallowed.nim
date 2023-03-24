discard """
description: "Variants are not allowed in unions"
errormsg: "Illegal use of ``case`` in union type."
line: 9
"""

type
  Disallowed {.union.} = object
    case foo: bool:
      of true:
        a: string
        b: int
      of false:
        c: float

var foo: Disallowed
doAssert sizeof(Disallow) == 8 # made up number, test should just fail