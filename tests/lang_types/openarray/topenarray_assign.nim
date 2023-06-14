discard """
  targets: "c js vm"
  matrix: "--experimental:views"
  description: "Tests for assigning first-class openArrays to locals"
"""

# Note the tests here are only intended to capture the current semantics and
# make sure they work; they're not intended to describe how things should be.
# Once the semantics of view-types are more fleshed out, the tests need to be
# adjusted

proc check(x: openArray[int]) =
  doAssert x[0] == 1
  doAssert x.len == 2

proc fromLocal() =
  # test: assign to ``openArray`` with implicit conversion from a sequence
  let s = @[1, 2]

  block:
    let x: openArray[int] = s
    check(x)

  # XXX: either crashes the compiler or generates invalid code --
  #      ``lent openArray`` doesn't make much sense in general.
  when false:
    let x: lent openArray[int] = s
    check(x)

  # XXX: currently required for the "outlives" analysis to not report
  #      an error
  discard s

fromLocal()

proc copyOpenarray() =
  # test: copying an ``openArray``
  let s = @[1, 2]
  let a: openArray[int] = s
  check(a)

  let b = a # copy the view
  doAssert b is openArray[int] # is the type correct?
  check(b)

  discard a
  discard s # XXX: work around "outlives" analysis issue

copyOpenarray()

proc copyMutableOpenarray() =
  # test: copying a ``var openArray``
  var s = @[1, 2]
  var a: var openArray[int] = s
  check(a)
  # modify `s` through the view
  a[1] += 1

  static:
    echo typeof(a)

  var b = a # copy the view
  doAssert b is openArray[int] # is the type correct?
  check(b)
  doAssert b[1] == 3 # is the modification visible through the view?
  b[1] += 2

  discard a
  # are the modifications visible at the source location?
  doAssert s[1] == 5

copyMutableOpenarray()

proc fromParam(oa: openArray[int]) =
  # test: assign an ``openArray`` parameter to a local
  let x = oa
  check(x)

fromParam([1, 2]) # test with array
fromParam(@[1, 2]) # test with seq

proc fromCall() =
  # test: assign the ``openArray`` result of a call to a local
  proc call(x: openArray[int]): openArray[int] =
    check(x)
    result = x # pass through

  let
    s = @[1, 2]
    v = call(s)
  check(v)

  discard s # XXX: work around "outlives"-analysis issue

fromCall()

proc mutable() =
  # test: create mutable view from a seq
  var s = @[1, 2]

  block:
    var x: var openArray[int] = s
    check(x)
    # verify that mutations are possible:
    x[0] = 3

  doAssert s[0] == 3

mutable()