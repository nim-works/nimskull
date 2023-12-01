discard """
  targets: c js vm
  description: '''
    Ensure that casting an int to a pointer, storing the pointer,
    and then casting the pointer back works.
  '''
"""

type
  Object = object
    f: pointer

# test with storing in a global variable
var i = 1
# test initial assignment
var global = cast[pointer](i)
doAssert cast[int](global) == 1 # cast-back works

# test with normal assignment
global = cast[pointer](i)
doAssert cast[int](global) == 1

proc test() =
  # test with local variable
  var local = cast[pointer](i)
  doAssert cast[int](local) == 1 # cast-back works

  local = cast[pointer](i)
  doAssert cast[int](local) == 1

test()

# test with storing in a parameter
proc test2(param: pointer) =
  doAssert cast[int](param) == 1 # cast-back works

test2(cast[pointer](i))

# test with storing in an aggregate type
var o = Object(f: cast[pointer](i))
doAssert cast[int](o.f) == 1 # cast-back works