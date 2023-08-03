discard """
  description: '''
    Invalid type arguments (not a type/static expression, etc.) also need to
    be detected and reported when using late instantiation
  '''
  cmd: "nim check --hints:off $options $file"
  action: reject
  nimout: '''
treject_invalid_type_arguments.nim(57, 7) Error: type mismatch: got <int literal(1)>
but expected one of:
proc test[A: int; B: float](x: static int)
  first type mismatch at position: 2
  missing parameter: A

expression: test[](1)
treject_invalid_type_arguments.nim(58, 10) Error: type mismatch: got <int literal(1)>
but expected one of:
proc test[A: int; B: float](x: static int)
  first type mismatch at position: 2
  missing parameter: B

expression: test[int](1)
treject_invalid_type_arguments.nim(59, 25) Error: type mismatch: got <int literal(1)>
but expected one of:
proc test[A: int; B: float](x: static int)
  first type mismatch at position: 0
  extra argument given

expression: test[int, float, 1, int](1)
treject_invalid_type_arguments.nim(63, 6) Error: cannot evaluate at compile time: x
treject_invalid_type_arguments.nim(64, 8) Error: invalid expression: A = int
treject_invalid_type_arguments.nim(67, 22) Error: type mismatch: got <int literal(1)>
but expected one of:
proc test[A: int; B: float](x: static int)
  first type mismatch at position: 0
  required type for A: A: int
  but expression '"string"' is of type: static[string]("string")

expression: test["string", float](1)
treject_invalid_type_arguments.nim(74, 22) Error: type mismatch: got <int literal(1)>
but expected one of:
proc test[A: int; B: float](x: static int)
  first type mismatch at position: 2
  required type for x:type: static[int]
  but expression 'int' is of type: typedesc[int]

expression: test[int, float, int](1)'''
"""

# using an implicit generic parameter forces late instantiation
proc test[A: int; B: float](x: static int) =
  discard

test[int, float](1) # works

# arity checks
test[](1)                   # missing parameter
test[int](1)                # missing parameter
test[int, float, 1, int](1) # extra argument

# a valid type expression is required
var x = 0
test[x, float](1)       # not a type expression
test[A = int, float](1) # not supported

# constraint matching
test["string", float](1)

# knownIssue: the following two should fail (a ``static[T]`` type must not
#             match a ``T`` constraint), but currently don't
test[2, float](1)
test[int, 1.0](1)

test[int, float, int](1) # mismatch: the lifted implicit parameter has to be a
                         # ``static[int]``