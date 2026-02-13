discard """
  description: '''
    Regression test for explicit conversions allowing conversion between
    unrelated proc types (that only differ in their effects)
  '''
  targets: native
"""

type Unknown = proc() # has all possible effects

block exception_effects:
  proc test1() = discard # no exception effects
  proc test2() {.raises: [CatchableError].} = discard
  proc test3() {.raises: [ValueError].} = discard
  proc test4() {.raises: [IOError].} = discard
  proc test5() {.raises: [ValueError, IOError].} = discard
  var test6: Unknown

  # "no effects" only matches "no effects"
  doAssert not compiles((typeof(test1))(test2))
  doAssert not compiles((typeof(test1))(test3))
  doAssert not compiles((typeof(test1))(test4))
  doAssert not compiles((typeof(test1))(test5))
  doAssert not compiles((typeof(test1))(test6))

  # "no effects" is a subtype of all other effects
  doAssert compiles((typeof(test2))(test1))
  doAssert compiles((typeof(test3))(test1))
  doAssert compiles((typeof(test4))(test1))
  doAssert compiles((typeof(test5))(test1))
  doAssert compiles((typeof(test6))(test1))

  # sub typing
  doAssert compiles((typeof(test2))(test3))
  doAssert compiles((typeof(test2))(test4))
  doAssert compiles((typeof(test2))(test5))
  doAssert not compiles((typeof(test3))(test2))
  doAssert not compiles((typeof(test4))(test2))
  doAssert not compiles((typeof(test5))(test2))

  # "all effects" only fits into "all effects"
  doAssert compiles(Unknown(test6))
  doAssert not compiles((typeof(test1))(test6))
  doAssert not compiles((typeof(test2))(test6))

block tag_effects:
  type
    TagA = object of RootEffect
    TagB = object of RootEffect

  # same structure as the exception effect tests above, just with tags
  proc test1() {.tags: [].} = discard
  proc test2() {.tags: [RootEffect].} = discard
  proc test3() {.tags: [TagA].} = discard
  proc test4() {.tags: [TagB].} = discard
  proc test5() {.tags: [TagA, TagB].} = discard
  var test6: Unknown

  # "no effects" only matches "no effects"
  doAssert not compiles((typeof(test1))(test2))
  doAssert not compiles((typeof(test1))(test3))
  doAssert not compiles((typeof(test1))(test4))
  doAssert not compiles((typeof(test1))(test5))
  doAssert not compiles((typeof(test1))(test6))

  # "no effects" is a subtype of all other effects
  doAssert compiles((typeof(test2))(test1))
  doAssert compiles((typeof(test3))(test1))
  doAssert compiles((typeof(test4))(test1))
  doAssert compiles((typeof(test5))(test1))
  doAssert compiles((typeof(test6))(test1))

  # sub typing
  doAssert compiles((typeof(test2))(test3))
  doAssert compiles((typeof(test2))(test4))
  doAssert compiles((typeof(test2))(test5))
  doAssert not compiles((typeof(test3))(test2))
  doAssert not compiles((typeof(test4))(test2))
  doAssert not compiles((typeof(test5))(test2))

  # "all effects" only fits into "all effects"
  doAssert compiles(Unknown(test6))
  doAssert not compiles((typeof(test1))(test6))
  doAssert not compiles((typeof(test2))(test6))
