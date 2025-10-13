discard """
  description: '''
    Various tests for operands to `static` constrained generic type parameters
    where the operand's type depends on unresolved type variables.
  '''
"""

type
  Type1[T: static] = object ## only must be *some* static value
  Type2[T: static int] = object ## must a be a static int

proc eval[T](x: T): T {.compileTime.} =
  x

proc p1[T](): Type1[eval(default(T))] = discard
proc p2[T](): Type2[eval(default(T))] = discard
# ^^ whether the ``Type2`` can be instantiated depends on the later
# supplied `T`

discard p1[int]()    # works
discard p1[float]()  # works
discard p1[string]() # works

discard p2[int]()    # int is convertible to int -> works
discard p2[float]()  # float is convertible to int -> works
# string is not convertible to float -> fails:
doAssert not compiles(p3[string]())
