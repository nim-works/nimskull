discard """
  action: reject
  description: '''
    Tests for inference failure of parameter types of procedural types during
    type matching
  '''
  cmd: "nim check $options --hints:off $file"
  nimout: '''tproc_type_inference_2.nim(61, 9) Error: type mismatch: got <proc [A](a: A)>
but expected one of:
proc call[T](x: proc (a: T))
  first type mismatch at position: 1
  required type for x: proc [T](a: T){.closure.}
  but expression 'generic' is of type: proc [A](a: A)

expression: call(generic)
tproc_type_inference_2.nim(70, 9) Error: type mismatch: got <proc [A](a: A)>
but expected one of:
proc call[T](x: proc (a: Container[T]))
  first type mismatch at position: 1
  required type for x: proc [T](a: Container[call.T]){.closure.}
  but expression 'generic' is of type: proc [A](a: A)

expression: call(generic)
tproc_type_inference_2.nim(77, 9) Error: type mismatch: got <proc [A](a: A)>
but expected one of:
proc call(x: proc (a: int | float))
  first type mismatch at position: 1
  required type for x: proc (a: int or float){.closure.}
  but expression 'generic' is of type: proc [A](a: A)

expression: call(generic)
tproc_type_inference_2.nim(88, 7) Error: type mismatch: got <proc [A](a: A, b: int)>
but expected one of:
proc call[T](x: proc (a: T; b: T))
  first type mismatch at position: 1
  required type for x: proc [T](a: T, b: T){.closure.}
  but expression 'generic' is of type: proc [A](a: A, b: int)

expression: call(generic)
tproc_type_inference_2.nim(97, 7) Error: type mismatch: got <proc [A](a: A, b: A)>
but expected one of:
proc call[T](x: proc (a: T; b: int))
  first type mismatch at position: 1
  required type for x: proc [T](a: T, b: int){.closure.}
  but expression 'generic' is of type: proc [A](a: A, b: A)

expression: call(generic)'''
"""

block generic_param_against_generic_param:
  # there's no match if both the formal and actual type are unresolved meta
  # types
  proc generic[A](a: A) = discard

  block both_generic_params:
    # the simple case: both the formal and actual type are unresolved generic
    # parameters
    proc call[T](x: proc(a: T)) =
      discard

    call(generic)
  
  block against_invocation:
    # the formal type is an unresolvable type invocation
    type Container[T] = object # <- deliberately a phantom type

    proc call[T](x: proc(a: Container[T])) =
      discard
    
    call(generic)

  block against_type_class:
    # the formal type is a type-class
    proc call(x: proc(a: int|float)) =
      discard

    call(generic)

block forward_inference_only:
  # inference in the context of procedural types is strictly forward. If both
  # the formal and actual type are unresolved generic parameters, there's no
  # match, even if the formal generic parameters could be inferred at a later
  # point
  proc call[T](x: proc(a: T, b: T)) = discard

  proc generic[A](a: A, b: int) = discard

  call(generic)

block forward_inference_only_2:
  # similar to the above, but here the *argument* generic parameter in the
  # type could be inferred later
  proc call[T](x: proc(a: T, b: int)) = discard

  proc generic[A](a: A, b: A) = discard

  call(generic)