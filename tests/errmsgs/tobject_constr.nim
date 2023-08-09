discard """
  description: '''
    Ensures that errors are correctly reported for literal object construction
    expressions
  '''
  cmd: "nim check --hints:off $file"
  action: reject
  nimoutFull: true
  nimout: '''
tobject_constr.nim(49, 15) Error: The fields 'd' and 'f' cannot be initialized together, because they are from conflicting branches in the case object.
tobject_constr.nim(52, 15) Error: a case selecting discriminator 'f' with value 'false' appears in the object construction, but the field(s) c are in conflict with this value.
tobject_constr.nim(35, 16) Error: undeclared field: 'v' for type tobject_constr.Object [type declared in tobject_constr.nim(25, 3)]
tobject_constr.nim(38, 22) Error: field initialized twice: 'a'
tobject_constr.nim(42, 22) Error: Invalid field assignment 'x'
tobject_constr.nim(42, 25) Error: Invalid field assignment 'y'
tobject_constr.nim(42, 28) Error: undeclared field: 'v' for type tobject_constr.Object [type declared in tobject_constr.nim(25, 3)]
tobject_constr.nim(45, 19) Error: type mismatch: got <string> but expected 'int'
tobject_constr.nim(45, 26) Error: undeclared identifier: 'v'
tobject_constr.nim(49, 19) Error: type mismatch: got <string> but expected 'bool'
tobject_constr.nim(52, 26) Error: Invalid field assignment 'v'
'''
"""

type
  Object = object
    a: int
    b: int
    case c: bool
    of false:
      d: int
    of true:
      f: int

# not an existing field:
discard Object(v: 0, a: 0)

# duplicate field assignment:
discard Object(a: 0, a: 1)

# incorrect syntax. Well-formed but semantically invalid
# assignments are still reported
discard Object(a: 0, x, y, v: 0)

# type error + error in assignemnt-source expression:
discard Object(a: "", b: v)

# discriminator assignment is invalid, but a "conflicting branch" can be and
# is still detected and reported
discard Object(c: "", d: 1, f: 2)

# incorrect syntax + conflicting selected branch:
discard Object(c: false, v, f: 2)