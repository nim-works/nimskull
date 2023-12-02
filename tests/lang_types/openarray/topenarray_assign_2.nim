discard """
  targets: "c js vm"
  matrix: "--experimental:views"
  description: "Tests for assigning first-class openArrays to object fields"
  knownIssue.c: "`cgen` produces illegal C code"
"""

type Object[T] = object
  x: openArray[T]

proc test[T, U](s: T, val: U) =

  block assignment_with_implicit_conversion:
    let o = Object[U](x: s)
    # try reading through the field
    doAssert o.x.len == 2
    doAssert o.x[1] == val

  var oa: openArray[U] = s # implicit conversion takes place
  block assignmnent_without_conversions:
    let o = Object[U](x: oa)
    doAssert o.x.len == 2
    doAssert o.x[1] == val

  # XXX: use `s` for the borrowing to work. This is a workaround that
  #      shouldn't be needed
  discard s

test([1, 2], 2) # test with an array
test(@[1, 2], 2) # test with a seq
test("12", '2') # test with a string