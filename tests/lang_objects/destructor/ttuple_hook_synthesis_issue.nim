discard """
  description: '''
    Ensure that hook synthesis considers ``(A, (B, C))`` different from
    ``(A, (B,), C)``.
  '''
  output: "3\n4\n1\n2\n"
  knownIssue.js vm: "seq destructors don't work yet"
"""

type Object = object
  val: int

proc `=destroy`(x: var Object) =
  echo x.val

# a seq is only used for testing purposes, since it caused misbehaviour at
# run-time. Using ``(Object, (Object,) Object)`` and
# ``(Object, (Object, Object))`` also led to the same issue, but ran
# "correctly" due to both types having the same in-memory layout
type
  Tup1 = (seq[(Object,)], Object)
  Tup2 = (seq[(Object, Object)],)

proc test() =
  var x: Tup1 = (@[(Object(val: 1),)], Object(val: 2))
  var y: Tup2 = (@[(Object(val: 3), Object(val: 4))],)
  # both locations were treated as having type `Tup1` by the called
  # destructor, resulting in either crashes or the wrong output being produced

test()
