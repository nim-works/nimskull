discard """
  targets: "c js !vm"
  description: '''
    A regression test to make sure that redefined locals don't reach (or at
    least cause issues with) the mid- and back-end
  '''
"""

# knownIssue: the ``injectdestructors`` pass is disabled for the VM target

# bug https://github.com/nim-lang/nim/issues/13622

type Obj = object
  val: int

proc `=destroy`(x: var Obj) =
  discard

template duplicate*(code: typed) {.dirty.} =
  code
  code

proc main() =
  var i = 0
  duplicate:
    inc i
    # use a type with a destructor in order to also verify that the
    # ``injectdestructors`` pass doesn't fail because of redefinitions
    var o = Obj(val: i) # <-- `o` is the duplicated symbol
    # test that accessing the local works
    doAssert o.val == i

  doAssert i == 2 # check that the block is executed twice

main()