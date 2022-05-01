discard """
  targets: "c cpp" # For good measure, also test with the C back-end
  matrix: "--gc:arc"
"""

# These are tests for the fix for the regressions introduced by PR
# https://github.com/nim-works/nimskull/pull/290.
# Since temporaries are only generated with arc/orc (see
# `ccgcalls.withTmpIfNeeded`) we explicitly build with `gc:arc` here

type T1 = object
  i: int

block:
  proc a(x: var T1, y: var int) =
    discard

  proc b(x: var T1) =
    a(x, x.i) # `nkHiddenAddr(nkHiddenDeref(`x`))` is collapsed to just `x`
              # after the PR mentioned above. Without a `nkHiddenAddr`, the
              # current implementation in `ccgcalls.genParams` forces
              # a temporary for `x` here, leading to faulty code being
              # generated

  var v: T1
  b(v)

block pass_through:
  proc a(x: var T1) = discard

  proc b(x: var T1) =
    a(x)

  var v: T1
  b(v)

block importc:

  # importc'ed procs with `nodecl` or `header` treat `var` params as `T*`
  # instead of `T&` in cpp mode. We emulate a function coming from C with
  # `pseudoCProc` here
  proc pseudoCProc(x: ptr int) {.exportc.} = discard
  proc cproc(x: var int) {.importc: "pseudoCProc", nodecl.}

  proc a(x: var int) =
    cproc(x) # `x` is a `T&` and needs to be correctly translated to `&x` here,
             # since `cproc` takes a `T*`

    var y = 1
    cproc(y) # this needs to still work too

  var v = 1
  a(v)

# Tests to make sure that the regression fix doesn't break other things

block:
  proc a(x: var int) = discard

  proc b(x: ptr int) =
    a(x[])

  var x: int
  b(addr x)

block:
  proc p(s: var seq[int]) =
    s = newSeq[int](1) #
      # arrives as `nkCall('=sink', nkHiddenAddr(nkHiddenDeref(s)), ...)` at
      # the back-end

  var s: seq[int]
  p(s)