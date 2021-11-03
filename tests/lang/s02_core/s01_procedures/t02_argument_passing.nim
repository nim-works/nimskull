discard """
description: '''
Different ways of passing arguments to procedures.
'''
"""

##


block different_number_of_arguments:
  ## Procedure can accept zero or more arguments.
  proc zero(): int = 1

  doAssert zero() == 1

  ## Arguments are specified in the procedure declaration
  proc single(arg: int): int = arg

  doAssert single(12) == 12
  doAssert single(2) == 2

  ## Multiple arguments can be specified either separately
  proc multi2(arg1: int, arg2: int): (int, int) = (arg1, arg2)

  doAssert multi2(1, 2) == (1, 2)
  doAssert multi2(3, 4) == (3, 4)

  ## Or together, if they have the same type
  proc multi3(arg1, arg2: int): (int, int) = (arg1, arg2)

  doAssert multi3(1, 2) == (1, 2)
  doAssert multi3(3, 4) == (3, 4)

# Putting different calling syntaxes in the middle of different argument
# modes because it is necessary to have one or more arguments passed, but
# is not really important
block regular_syntax:
  ## Procedure can be called with different
  proc regular(arg: int) = discard

  regular(12)


block command_syntax:
  proc command(arg: int) = discard

  command 12

  ## Note that due to UFCS whitespace separating function name from the
  ## arguments is important. In this case we have two procedures - one that
  ## accepts tuple, and other for two arguments.
  proc takesTuple(arg: (int, int)) = discard
  proc takesTwoArguments(arg1, arg2: int) = discard

  ## Adding whitespace before parens will be considered calling proc via
  ## command syntax and passing tuple as an argument.
  takesTuple (1, 2)

  ## No leading whitespace in this case will be considered a call with
  ## regular syntax
  takesTwoArguments(1, 2)

block block_syntax:
  ## If needed, expression for arguments can be placed on the
  ## multiple lines using block call syntax.
  proc blockProc(arg: int) = discard

  ## Block call syntax using parens
  blockProc():
    12

  ## Block call with command syntax
  blockProc:
    12

  proc blockReturn(arg: int): int = arg

  ## Block syntax for procedure calls can also be used as an expression,
  ## although no in the infix expressions (like `doAseert 12 == <block here>`)
  block:
    let res = blockReturn():
      12

    doAssert res == 12

  block:
    let res = blockReturn:
      12

    doAssert res == 12

block block_syntax_do:
  ## If there are multiple arguments that have to be placed on the
  ## separate lines this can be achieved using `do:` for
  ## the arguments.
  proc blockProc(arg1: int, arg2: int): (int, int) = (arg1, arg2)

  block:
    let res = blockProc():
      12

    do:
      23

    doAssert (12, 23) == res

  block:
    let res = blockProc:
      12

    do:
      20

    doAssert (12, 20) == res

  ## Procedures called with block syntax as also be called using regular
  ## command or parenthised syntax.
  doAssert (1, 2) == blockProc(1, 2)

  ## It is also possible to supply only certain arguments.
  block:
    let res = blockProc(1123):
      20

    doAssert (1123, 20) == res


  block:
    let res = blockProc 1:
      20

    doAssert (1, 20) == res


block named_argument_call:
  proc named(arg1, arg2: int): (int, int) = (arg1, arg2)

  doAssert named(1, 2) == (1, 2)
  doAssert named(arg1 = 1, arg2 = 2) == (1, 2)
  doAssert named(arg2 = 2, arg1 = 1) == (1, 2)


block default_argument:
  proc withDefault(arg: int = 12): int =
    return arg

  doAssert withDefault() == 12
  doAssert withDefault(23) == 23

  proc withDefaultInfer(arg = 12): int =
    return arg

  doAssert withDefaultInfer() == 12
  doAssert withDefaultInfer(30) == 30


block named_argument_with_default:
  ## It is possible to call procedure with default argument value
  ## using named argument passing even regular ordering would've
  ## prevented this


block default_and_regular_argument:
  proc withDefault(arg1: int, arg2: string = "123"): (int, string) =
    return (arg1, arg2)

  doAssert withDefault(1) == (1, "123")
  doAssert withDefault(2, "1") == (2, "1")

block multiple_default_arguments:
  proc withDefaultSplit(arg1: int = 1, arg2: int = 2): (int, int) =
    return (arg1, arg2)

  doAssert withDefaultSplit() == (1, 2)
  doAssert withDefaultSplit(9) == (9, 2)
  doAssert withDefaultSplit(3, 4) == (3, 4)


  proc withDefaultMerged(arg1, arg2: int = 12): (int, int) = (arg1, arg2)

  doAssert withDefaultMerged() == (12, 12)
  doAssert withDefaultMerged(9) == (9, 12)
  doAssert withDefaultMerged(3, 4) == (3, 4)

block complex_expression_as_default:
  block use_other_argument:
    proc useOther(arg1: int, arg2: int = arg1): (int, int) = (arg1, arg2)

    doAssert useOther(1) == (1, 1)
    doAssert useOther(1, 2) == (1, 2)


  block use_field_of_other_argument:
    type
      Object = object
        field: int

    proc useOther(obj: Object, arg: int = obj.field): (int, int) = (obj.field, arg)

    doAssert useOther(Object(field: 1)) == (1, 1)
    doAssert useOther(Object(field: 1), 2) == (1, 2)


  block call_procedure:
    proc getValue(): int = 12

    proc useOther(arg: int = getValue()): int = arg

    doAssert useOther(getValue()) == useOther()

    doAssert useOther() == 12
    doAssert useOther(1) == 1

  block evaluate_block_expression:
    proc useOther(
      arg: int = 12,
      other: int =
        block:
          arg
    ): (int, int) = (arg, other)

    doAssert useOther() == (12, 12)
    doAssert useOther(2) == (2, 2)
    doAssert useOther(1, 2) == (1, 2)




block varargs_arguments_no_converter:
  block single_varargs:
    proc impl(vas: varargs[int]): seq[int] = @vas


    doAssert impl() == newSeq[int]()
    doAssert impl(1) == @[1]
    doAssert impl(1, 2, 4, 5, 6, 7) == @[1, 2, 4, 5, 6, 7]

  # Multiple varargs is most likely a feature that is not used widely
  # (if at all), but this is stil supported, so should probably be tested.
  block multiple_varargs:
    proc impl(va1: varargs[int], va2: varargs[bool]): (seq[int], seq[bool]) =
      return (@va1,  @va2)

    doAssert impl(1) == (@[1], newSeq[bool]())

    if false:
      # Right nor results in `(@[1, 2, 0], @[true, false, false])`
      doAssert impl(1, 2, false) == (@[1, 2], @[false])

    ## It is not possible to entirely omit leading varargs arguments, but it
    ## is possible to explicitly pas values to the other varargs if present
    doAssert impl(va2 = true) == (newSeq[int](), @[true])


  block command_syntax:
    proc vaProc(args: varargs[int]): seq[int] = @args

    block:
      # Command syntax cannot be used in the `let val = cmd 1,2,3,4,4`
      #
      # TODO REVIEW is it a bug? https://github.com/nim-works/nimskull/pull/28#discussion_r739863281
      let val =
        block:
          vaProc 1, 2, 3

      doAssert val == @[1,2,3]

    block:
      ## When used without any arguments in the `let val = vaProc`
      ## expressions, procedure itself gets assigned to the variable.
      let val = vaProc
      doAssert val is proc


  block regular_syntax:
    proc vaProc(args: varargs[int]): seq[int] = @args

    doAssert vaProc(1, 2, 3) == @[1, 2, 3]
    doAssert vaProc(1, 2) == @[1, 2]
    doAssert vaProc() == newSeq[int]()

  block block_syntax:
    proc vaProc(args: varargs[int]): seq[int] = @args

    doAssert():
      `==`():
        @[12]

      do:
        vaProc():
          12

  block block_syntax_do:
    proc vaProc(args: varargs[int]): seq[int] = @args

    doAssert():
      `==`():
        @[12, 13]

      do:
        vaProc():
          12

        do:
          13


block varargs_arguments_with_converter:
  proc convertToInt(arg: float): int = int(arg)

  # TODO REVIEW revisit the interaction here because this can seriously
  # mess up overload resolution.
  #
  # https://github.com/nim-works/nimskull/pull/28#discussion_r739863459
  proc vaWithConverter(args: varargs[int, convertToInt]): seq[int] = @args


block immutable_arguments:
  # TODO IMPLEMENT
  discard

block mutable_arguments:
  # TODO IMPLEMENT
  discard
