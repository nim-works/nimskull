discard """
ccodecheck: "\\i !@('struct tyObject_MyRefObject'[0-z]+' {')"
ccodecheck: "\\i !@('mymoduleInit')"
output: "hello"
"""

# issue #7339 
# Test that MyRefObject is only forward declared as it used only by reference

import mymodule
type AnotherType = object
  f: MyRefObject 

let x = AnotherType(f: newMyRefObject("hello"))
echo $x.f

# assigning the ref (outside of a var/let statement) must not pull in the
# type definition
proc test() =
  var y: MyRefObject
  y = newMyRefObject("abc")
  doAssert $y == "abc"

test()

# bug #7392
var x1: BaseObj
var x2 = ChildObj(x1)
