discard """
  description: "Tests for the '.compileTime' pragma"
  targets: "c js vm"
"""

template test(global: int) {.dirty.} =
  block:
    # make sure that the global can be accessed in a compile-time context
    static:
      doAssert global == 1
      global = 2

    const c = global
    static:
      doAssert c == 2

# top-level global
var g {.compileTime.} = 1
test(g)

block static_global:
  static:
    var inner {.compileTime.} = 1 # the pragma is a no-op
    doAssert inner == 1
    inner = 2
    doAssert inner == 2

block procedure_global:
  proc p() =
    var pg {.compileTime, global.} = 1
    test(pg)

    when false: # XXX: doesn't work because of an unrelated issue
      static:
        # the procedure itself is not compile-time-only, but `inner` is defined in a
        # compile-time-only context, so the `.compileTime` is valid (and a no-op)
        var inner {.compileTime.} = 1
        doAssert inner == 1
        inner = 2
        doAssert inner == 2

  static:
    p() # also has to work when `p` is executed at compile-time
  p()

block compiletime_proc:
  proc p() {.compileTime.} =
    var local {.compileTime.} = 1 # the pragma is treated as a no-op
    doAssert local == 1
    local = 2
    doAssert local == 2

  static:
    # call the procedure twice to make sure that `local` is really a `local`
    p()
    p()

block compiletime_macro:
  macro m() = # a macro is implicitly a compile-time procedure
    var local {.compileTime.} = 1 # the pragma is a no-op
    doAssert local == 1
    local = 2
    doAssert local == 2

  m()
  m()