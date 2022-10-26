discard """
  description: "Various tests for closures"
  action: compile
"""


# XXX: not specific to the VM
block nimcall_closure_conversion:

  proc a() =
    proc p() {.nimcall.} = discard
    # assign a proc of nimcall convention to a closure
    var x: proc() {.closure.} = p

  static:
    a()

block nil_closure:
  static:
    var a: proc() {.closure.} = nil
    doAssert a.isNil

# XXX: not specific to the VM
block env_mutation1:
  proc a() =
    var val = 0
    proc mutate() =
      val = 1

    mutate()
    doAssert val == 1

  static:
    a()

# XXX: not specific to the VM
block env_mutation2:
  proc a(): proc(): int =
    var e = 0
    result = proc(): int =
      e += 1
      result = e

  static:
    let c = a()
    let c2 = c

    doAssert c() == 1
    doAssert c2() == 2
    doAssert c() == 3


# XXX: not specific to the VM
block closure_assign:
  type
    Obj = object
      a, b: string
    Obj2 = object
      a, b: int

  proc mkClosure1(): proc() =
    var e: Obj
    e.a = "a"
    e.b = "b"

    result = proc () =
      doAssert e.a == "a"
      doAssert e.b == "b"

  proc mkClosure2(): proc() =
    var e: Obj2
    e.a = 1
    e.b = 2

    result = proc () =
      doAssert e.a == 1
      doAssert e.b == 2

  # Variables at the scope of top-level static blocks are treated as
  # globals. Test closure assignment once in a top-level context and once
  # inside a function

  proc p() =
    var cl = mkClosure1()
    cl()

    # Assign a closure with a different environment type
    cl = mkClosure2()
    cl()

  static:
    p()

    # Top-level test (cl is a global)
    var cl = mkClosure1()
    cl()

    # Assign a closure with a different environment type
    cl = mkClosure2()
    cl()