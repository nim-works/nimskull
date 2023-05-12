discard """
  targets: "c js !vm"
  description: "Tests for ensuring that ``FieldDefect``'s messages are correct"
"""

# knownIssue: ``FieldDefect``s are not currently catchable with the VM target,
#             not even for debugging purposes

type
  Enum = enum
    e1
    e2

  Object = object
    case boolKind: bool
    of false: a: int
    of true:  discard

    case charKind: char
    of 'a': b: int
    else:   discard

    case enumKind: Enum
    of e1: c: int
    of e2: discard

    case intKind: range[0..1]
    of 0: d: int
    of 1: discard

    case uintKind: range[0'u..1'u]
    of 0: e: int
    of 1: discard

var obj = Object(boolKind: true, charKind: 'b', enumKind: e2, intKind: 1,
                 uintKind: 1)

template test(field: untyped, expect: string) =
  try:
    # write something to the field in order to make sure it's really accessed
    obj.field = 1
  except FieldDefect as e:
    doAssert e.msg == expect, "got: " & e.msg

test(a, "field 'a' is not accessible for type 'Object' using 'boolKind = true'")
test(b, "field 'b' is not accessible for type 'Object' using 'charKind = 98'")
# XXX: the runtime doesn't support rendering characters in field defect
#      messages yet
test(c, "field 'c' is not accessible for type 'Object' using 'enumKind = e2'")
test(d, "field 'd' is not accessible for type 'Object' using 'intKind = 1'")
test(e, "field 'e' is not accessible for type 'Object' using 'uintKind = 1'")