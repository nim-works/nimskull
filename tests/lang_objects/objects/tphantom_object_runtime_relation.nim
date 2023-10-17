discard """
  targets: "c js vm"
  description: "Ensure that the 'of' operator takes phantom types into account"
"""

type
  PhantomBase[T] = object of RootObj

  Sub1 = object of PhantomBase[int]
  Sub2 = object of PhantomBase[float]

var
  s1: Sub1
  s2: Sub2

# static checks (the `of` operation can and is evaluated at compile-time):
doAssert s1 of PhantomBase[int]
doAssert s2 of PhantomBase[float]
doAssert not(s1 of PhantomBase[float])
doAssert not(s2 of PhantomBase[int])

var
  r1: RootRef = new(Sub1)
  r2: RootRef = new(Sub2)

# dynamic checks (the location's static type is not known at compile-time):
doAssert r1 of PhantomBase[int]
doAssert r2 of PhantomBase[float]

template check(cond: bool) =
  when defined(c):
    # XXX: the tests don't fail at the moment, as the generated names
    #      don't include the instantiated-with parameters
    doAssert(not cond, "run-time relation works as it should")
  else:
    # works properly with the other backends
    doAssert cond

check not(r1 of PhantomBase[float])
check not(r2 of PhantomBase[int])