discard """
  description: "Tests to make sure that the Exception.name field is set"
"""

template test(desc, body) {.dirty.} =
  block:
    # Don't execute at top level
    proc wrapper() =
      body

    wrapper()


test "Valid name without raising first":
  let e1 = IOError.newException("")
  doAssert e1.name == "IOError"

  let e2 = ValueError.newException("")
  doAssert e2.name == "ValueError"


test "Valid name after raising":

  proc raiseTest[T](typ: typedesc[T], name: string) =
    try:
      raise T.newException("")
    except T as e:
      doAssert e.name == name

  raiseTest(IOError, "IOError")
  raiseTest(ValueError, "ValueError")


test "Empty name":

  let e = (ref CatchableError)()
  doAssert e.name == nil


test "Name is set on raise if it was unset":
  try:
    raise (ref IOError)() # leave `name` as `nil`
  except IOError as e:
    doAssert e.name == "IOError"

test "Don't override an explicitly empty name":
  try:
    raise (ref IOError)(name: "") # explicitly set name to an empty string
  except IOError as e:
    when defined(vm):
      # see ``texception_name_vm_issue.nim``
      doAssert e.name != "", "works now"
    else:
      doAssert e.name == ""