discard """
  description: '''
    Ensure that fields whose type is substituted with `void` are properly
    removed from tuple types during instantiation
  '''
"""

type
  Anon[A, B, C] = (A, B, C)
  Named[A, B, C] = tuple[a: A, b: B, c: C]

block trailing_field_is_void:
  var x: Anon[int, string, void]
  doAssert x is (int, string)
  doAssert sizeof(x) == sizeof((int, string))
  doAssert not compiles(x[2])
  # make sure the fields are accessible:
  x[0] = 1
  x[1] = ""

  # with generic named tuple:
  var y: Named[int, string, void]
  doAssert y is tuple[a: int, b: string]
  doAssert sizeof(y) == sizeof((int, string))
  doAssert not compiles(y.c)
  # make sure the fields are accessible:
  y.a = 1
  y.b = ""

block middle_field_is_void:
  var x: Anon[int, void, string]
  doAssert x is (int, string)
  doAssert sizeof(x) == sizeof((int, string))
  doAssert not compiles(x[2])
  # make sure the fields are accessible:
  x[0] = 1
  x[1] = ""

  # with generic named tuple:
  var y: Named[int, void, string]
  doAssert y is tuple[a: int, c: string]
  doAssert sizeof(y) == sizeof((int, string))
  doAssert not compiles(y.b)
  y.a = 1
  y.c = ""

block all_fields_are_void:
  var x: Anon[void, void, void]
  doAssert sizeof(x) == 1
  doAssert not compiles(x[0])

  var y: Named[void, void, void]
  doAssert sizeof(y) == 1
  doAssert not compiles(y.a)

block non_generic_field:
  type Tup[T] = tuple[a: T, b: int]

  # instantiate the type with a non-void parameter first
  var x: Tup[string]
  x.a = ""
  x.b = 0

  # instantiating the type with a void parameter must not affect previous
  # instantiations (code generation would fail if it does)
  var y: Tup[void]
  y.b = 0
