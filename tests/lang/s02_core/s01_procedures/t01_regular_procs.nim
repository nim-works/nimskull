discard """
description: '''
Test procedure definition, return values, forward declarations
'''
"""

## Language feature documentation.


block return_value:
  proc explicitReturn(): int =
    return 12

  proc assignReturn(): int =
    result = 12

  proc implicitReturn(): int =
    12

  proc noReturn(): int =
    discard

  doAssert explicitReturn() == 12
  doAssert assignReturn() == 12
  doAssert implicitReturn() == 12
  doAssert noReturn() == 0

block return_void:
  proc explicitReturn(): void =
    return

  proc noReturn(): void =
    discard

  proc explicitReturnNoVoid() =
    return

block t_return_tuple:
  proc returnTuple(): (int, int) = (1, 2)
  proc returnNamedTuple(): tuple[name1: int, name2: int] = (13, 12)

  doAssert returnTuple() == (1, 2)
  doAssert returnNamedTuple() == (13, 12)
  doAssert returnNamedTuple().name1 == 13
  doAssert returnNamedTuple().name2 == 12

  ## When returned from procedure named tuples still support all regular tuple operations,
  let named = returnNamedTuple()

  ## Such as index-based access
  doAssert named[0] is int
  doAssert named[0] == 13
  doAssert named is tuple[name1: int, name2: int]
  doAssert named isnot tuple[other1: int, other2: int]
  doAssert named is (int, int)


  ## Access by field name
  doAssert named.name1 == 13
  doAssert named.name2 == 12

  ## And unpacking.
  let (unpack1, unpack2) = returnNamedTuple()
  doAssert unpack1 == 13
  doAssert unpack2 == 12

block t_accept_argument:
  ## Procedure can accept one or more argument. For more tests/texamples with different
  ## modess of argument passing see `t02_argument_passing.nim`.
  proc acceptArgument(arg: int): int =

    ## Passed arguments can be accessed directly within a procedure body.
    return arg

  doAssert acceptArgument(12) == 12
  doAssert acceptArgument(0) == 0


block forward_declare:
  proc forward(): int

  proc callsForward(): int = forward()

  proc forward(): int = 12


  doAssert forward() == 12
  doAssert callsForward() == 12


block declare_nested:
  proc main(): int =
    proc nested(): int = 12

    doAssert nested() == 12

    return nested()

  doAssert main() == 12

block forward_declare_nested:
  proc main(): int =
    proc nested(): int

    proc callsForward(): int = nested()

    proc nested(): int = 12

    doAssert nested() == 12
    doAssert callsForward() == 12

    return callsForward()

  doAssert main() == 12


block forward_declare_mutually_recurisve:
  discard

block nested_declare_capture:
  discard

block nested_declare_capture_mutable:
  discard

block control_flow_return:
  discard

block control_flow_defer:
  discard
