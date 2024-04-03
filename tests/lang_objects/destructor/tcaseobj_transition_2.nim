discard """
  description: '''
    Ensure that the destructor of a field within a case obj is triggered when
    changing the active case object branch
  '''
  targets: "c js vm"
"""

type
  Object = object
    name: int

var steps: seq[int]

proc `=destroy`(x: var Object) =
  steps.add x.name

type
  WithVariant = object
    outer: Object
      ## field not within the case object; unaffected by branch changing
    case kind: bool
    of false:
      a: Object
    of true:
      b: Object

proc test() =
  var o = WithVariant(outer: Object(name: 1), kind: false, a: Object(name: 2))
  o.kind = true # change branch; `a` is destroyed
  doAssert o.b.name == 0 # uninitialized
  o.b.name = 3
  # on destruction, both `b` and `outer` must be destroyed

test()
doAssert steps == [2, 1, 3], $steps