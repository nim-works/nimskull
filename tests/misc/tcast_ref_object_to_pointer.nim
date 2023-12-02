discard """
  targets: c js vm
  description: '''
    Ensure that casting a ``ref object`` to a pointer, storing the pointer,
    and then casting the pointer back works.
  '''
  knownIssue.vm: "Casting from ``pointer`` to ``ref`` is not yet supported"
"""

type
  Ref = ref object
    x: int

  Object = object
    f: pointer

# test with storing in a global variable
var r = Ref(x: 1)
# test initial assignment
var global = cast[pointer](r)
doAssert cast[Ref](global).x == 1 # cast-back works

# test with normal assignment
global = cast[pointer](r)
doAssert cast[Ref](global).x == 1

proc test() =
  # test with local variable
  var local = cast[pointer](r)
  doAssert cast[Ref](local).x == 1 # cast-back works

  local = cast[pointer](r)
  doAssert cast[Ref](local).x == 1

test()

# test with storing in a parameter
proc test2(param: pointer) =
  doAssert cast[Ref](param).x == 1 # cast-back works

test2(cast[pointer](r))

# test with storing in an aggregate type
var o = Object(f: cast[pointer](r))
doAssert cast[Ref](o.f).x == 1 # cast-back works