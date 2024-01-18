discard """
  targets: c js vm
  description: '''
    Ensure that casting a ref-to-scalar type (``ref int`` in this case) to a
    pointer, storing the pointer, and then casting the pointer back works.
  '''
  knownIssue.vm: "Casting from ``pointer`` to ``ref`` is not yet supported"
"""

type
  Ref = ref int

  Object = object
    f: pointer

# test with storing in a global variable
var r = new Ref
r[] = 1
# test initial assignment
var global = cast[pointer](r)
doAssert cast[Ref](global)[] == 1 # cast-back works

# test with normal assignment
global = cast[pointer](r)
doAssert cast[Ref](global)[] == 1

# test with local variable
proc test() =
  var local = cast[pointer](r)
  doAssert cast[Ref](local)[] == 1 # cast-back works

  local = cast[pointer](r)
  doAssert cast[Ref](local)[] == 1

test()

# test with storing in a parameter
proc test2(param: pointer) =
  doAssert cast[Ref](param)[] == 1 # cast-back works

test2(cast[pointer](r))

# test with storing in an aggregate type
var o = Object(f: cast[pointer](r))
doAssert cast[Ref](o.f)[] == 1 # cast-back works