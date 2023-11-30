discard """
  targets: "c js vm"
  description: "Ensure that taking the address of up-conversions works"
"""

type
  Base = object of RootObj
    value: int
  Sub    = object of Base

block addr_of_object:
  let v1 = Sub(value: 1) # try with let
  # assign to a variable so that taking the address is not folded away
  var p = addr Base(v1)
  doAssert p[].value == 1 # make sure that reading through the pointer works

  var v2 = v1 # try with var
  p = addr Base(v2)
  doAssert p[].value == 1

block addr_of_ref:
  # same as the above, but with ref types
  let v1 = (ref Sub)(value: 1)
  var p = addr (ref Base)(v1)
  doAssert p[].value == 1

  var v2 = v1
  p = addr (ref Base)(v2)
  doAssert p[].value == 1
