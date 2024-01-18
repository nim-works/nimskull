discard """
  targets: "c js vm"
  description: '''
    Ensure that calls to compile-time-only procedures with side-effects are
    not folded when appearing within other compile-time-only contexts
  '''
"""

# XXX: the test is unrelated to the VM, it needs to be moved into a different
#      category

var state {.compileTime.} : int = 0

proc get(): int {.compileTime.} =
  # `get` accesses global state and is thus not pure
  result = state

# try with a static expression
doAssert static((inc state; get())) == 1

# try with a static statement
static:
  inc state
  doAssert get() == 2

# try with the intializer of a constant
const C0 = (inc state; get())
doAssert C0 == 3

# try with another compile-time-only proc
proc other(): int {.compileTime.} =
  inc state
  result = get()

# for this test, don't rely on `other` being automatically folded when used in
# a non-compile-time-only context
const C1 = other()
doAssert C1 == 4

# try with a macro:
macro m() =
  inc state
  doAssert get() == 5

m()

# verify that the compile-time-only procedure can be used in run-time
# contexts
doAssert other() == 6
doAssert other() == 7