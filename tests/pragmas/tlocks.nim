
type SomeBase* = ref object of RootObj
type SomeDerived* = ref object of SomeBase
  memberProc*: proc ()

method testMethod(g: SomeBase) {.base, locks: "unknown".} = discard
method testMethod(g: SomeDerived) =
  if g.memberProc != nil:
    g.memberProc()

# ensure int literals still work
proc plain*() {.locks: 0.} =
  discard

# ensure unknown locklevel works
proc plain2*() {.locks: "unknown".} =
  discard

# ensure char literals are rejected
doAssert not compiles (;
  proc plain3() {.locks: 'c'.} =
    discard
  )
