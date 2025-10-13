discard """
  description: '''
    Ensure that the in-place aggregate construction optimization doesn't affect
    observable semantics
  '''
"""

# assignments are always evaluated left-to-right. Optimizing an assignment with
# a constructed rvalue as the source into an in-place construction is
# allowed, as long as this doesn't lead to left-to-right evaluation order
# violations

proc get[T](x: T): T = x

block objects:
  type Obj = object
    a, b: int

  var obj = Obj(a: 1, b: 2)
  # direct usage as a source value:
  obj = Obj(a: 3, b: obj.a)
  doAssert obj == Obj(a: 3, b: 1)

  # lhs used in the construction expression:
  obj = Obj(a: 4, b: (let v = obj.a; v))
  doAssert obj == Obj(a: 4, b: 3)

  # lhs used as an argument to a call in the construction expression:
  obj = Obj(a: 5, b: get(obj.a))
  doAssert obj == Obj(a: 5, b: 4)

block anonymous_tuples:
  var atup = (1, 2)
  # direct usage as a source value:
  atup = (3, atup[0])
  doAssert atup == (3, 1)

  # lhs used in the construction expression:
  atup = (4, (let v = atup[0]; v))
  doAssert atup == (4, 3)

  # lhs used as an argument to a call in the construction expression:
  atup = (5, get(atup[0]))
  doAssert atup == (5, 4)

block named_tuples:
  var ntup = (x: 1, y: 2)
  # direct usage as a source value:
  ntup = (x: 3, y: ntup.x)
  doAssert ntup == (x: 3, y: 1)

  # lhs used in the construction expression:
  ntup = (x: 4, y: (let v = ntup.x; v))
  doAssert ntup == (x: 4, y: 3)

  # lhs used as an argument to a call in the construction expression:
  ntup = (x: 5, y: get(ntup.x))
  doAssert ntup == (x: 5, y: 4)

block arrays:
  var arr = [1, 2]
  # direct usage as a source value:
  arr = [3, arr[0]]
  doAssert arr == [3, 1]

  # lhs used in the construction expression:
  arr = [4, (var v = arr[0]; v)]
  doAssert arr == [4, 3]

  # lhs used as an argument to a call in the construction expression:
  arr = [5, get(arr[0])]
  doAssert arr == [5, 4]

block sets:
  # set elements cannot be directly as input elements in the
  # construction, but the assigned-to set can be queried while
  # evaluating the construction, in which case in-place
  # construction must not be used
  var s = {1'u8, 3'u8}
  s = {2'u8, (doAssert 2 notin s; 4'u8)}

block object_callee:
  # contrived case: an object member is used as the callee on the right-hand
  # side of an object construciton assignment
  type Obj = object
    a: proc(): int
    b: int

  var obj = Obj(a: proc (): int = 1)
  # if in-place construction is used, a nil-access defect will occur
  obj = Obj(a: nil, b: obj.a())
  doAssert obj == Obj(a: nil, b: 1)
