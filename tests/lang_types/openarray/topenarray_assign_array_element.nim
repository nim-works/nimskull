discard """
  targets: "c js vm"
  matrix: "--experimental:views"
  description: "Tests for constructing arrays containing first-class openArrays"
"""

proc test[T, U](s: T, val: U) =

  block construction_with_conversion:
    let o = [openArray[U](s)]
    # try reading through the field
    doAssert o[0].len == 2
    doAssert o[0][1] == val

  var oa: openArray[U] = s # implicit conversion takes place
  block construction_without_conversion:
    let o = [oa]
    doAssert o[0].len == 2
    doAssert o[0][1] == val

  # XXX: use `s` for the borrowing to work. This is a workaround that
  #      shouldn't be needed
  discard s

static:
  test([1, 2], 2) # test with an array
  test(@[1, 2], 2) # test with a seq
  test("12", '2') # test with a string