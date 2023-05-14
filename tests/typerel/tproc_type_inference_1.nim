discard """
  description: '''
    Tests for inference of parameter types of procedural types during type
    matching
  '''
"""

block inferred_actual_type:
  # passing a generic routine to a parameter of concrete procedural type
  # works; the types are inferred and an instantiation created
  proc call(x: proc(x: int): bool) =
    doAssert x(1) == true

  proc generic[A, B](x: A): B =
    result = true

  call(generic)
  # also works for lambda expressions:
  call(proc(x: auto): bool = true)

block inferred_actual_type_late_bound:
  # inference also works when the formal type is a type variable that has a
  # type bound already
  proc call[T](a: T, x: proc(x: T): bool) =
    doAssert x(a) == true

  proc generic[A, B](x: A): B =
    result = true

  call(1, generic)
  # also works for lambda expressions:
  call(2, proc(x: auto): bool = true)

block cross_inference:
  # the inferred type is bound to the type variable of the actual parameter,
  # meaning that each further usage of the type variable acts like the type it
  # was inferred as
  proc call[A, B](val: A, x: proc(a: A, b: B): bool) =
    doAssert x(val, val) == true

  proc generic[A](a: A, b: A): bool =
    result = true

  call(1, generic)
  # a lambda expression with two parameters using the same type variable
  # (i.e., generic parameter) is not supported by the parser

block more_complex_inference:
  # inference also works when both the formal and actual type are not directly
  # generic parameters
  type
    Container[T] = object
    Alias[U]     = Container[U]

  proc call[T](val: T, x: proc(a: Container[T]): bool) =
    doAssert x(Container[T]()) == true

  proc generic[T](a: Alias[T]): bool =
    result = true

  call(1, generic)

block auto_return_type:
  # a generic routine using an 'auto' return type can also be passed to a
  # parameter of procedural type. All type variables of the input procedural
  # type are inferred first, and then the routine is instantiated. After
  # that, the type of the instantiated routine is matched against the formal
  # type
  proc callSimple(val: int, x: proc(x: int): int) =
    doAssert x(val) == val

  proc callGenericRet[T](val: int, x: proc(x: int): T): T =
    # a type variable in the return type slot of the formal procedural type can
    # be inferred if the argument type has an auto return type
    result = x(val)

  proc generic[T](x: T): auto =
    result = x

  callSimple(1, generic)
  doAssert callGenericRet(2, generic) == 2
  # lambda expressions also support the auto return type
  callSimple(3, proc(x: auto): auto = x)
  doAssert callGenericRet(3, proc(x: auto): auto = x) == 3

block complex_argument_expression_with_auto_return_type:
  # complex expressions yielding generic routines also work
  proc call(val: int, x: proc(x: int): int) =
    doAssert x(val) == val

  proc generic[T](x: T): auto =
    result = x

  # with statement expression lists:
  call(1, (discard ""; generic))
  # also works with lambda expressions:
  call(2, (discard ""; (proc(x: auto): auto = x)))

  # with block expressions:
  call(1):
    block: discard ""; generic
  # also works with lambda expressions:
  call(2):
    block: discard ""; (proc(x: auto): auto = x)