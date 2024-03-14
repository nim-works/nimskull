discard """
description: '''
Covers the multitude of ways you can pass arguments to procedures.
'''
"""

# TODO: break things out as more advanced sections:
# - variadics should be their own spec
#   - variadic with conversion, is yet more complicated
# - syntax: separate dot call rewrites
# - named arguments
# - default arguments


block different_number_of_arguments:
  ## Procedures can accept zero or more arguments.
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

  ## Note that due to UFCS whitespace, separating a procedure name from the
  ## arguments changes behaviour. In this case we have two procedures - one that
  ## accepts a tuple, and the other accepts two arguments.
  proc takesTuple(arg: (int, int)) = discard
  proc takesTwoArguments(arg1, arg2: int) = discard

  ## Adding whitespace before the parenthesis will be considered calling the
  ## procedure/function via command syntax; therefore, we are passing the tuple
  ## as an argument.
  takesTuple (1, 2)

  ## No leading whitespace in this case will be considered a call with
  ## regular syntax; therefore, we are passing two separate arguments.
  takesTwoArguments(1, 2)

block block_syntax:
  ## If needed, expressions for arguments can be placed on
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
  ## although not in infix expressions (like `doAssert 12 == <block here>`)
  block:
    let res = blockReturn():
      12

    doAssert res == 12

  block:
    let res = blockReturn:
      12

    doAssert res == 12

block block_syntax_do:
  ## If there are multiple arguments that have to be placed on
  ## separate lines then you can use `do:` notation for
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

  ## Procedures called with block syntax can also be called using regular
  ## command or parenthesis syntax.
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
  ## It is possible to call procedures with default argument values
  ## using named argument passing; even regular ordering would've
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

    ## It is not possible to entirely omit leading varargs arguments; however, it
    ## is possible to explicitly pass values to the other varargs (if present)
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
      ## When used without any arguments/parenthesis in a an assignment
      ## expression, then the procedure itself gets assigned to the variable.
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
  var used = 0
  proc convertToInt(arg: float): int =
    inc used
    int(arg)

  # TODO REVIEW revisit the interaction here because this can seriously
  # mess up overload resolution.
  #
  # https://github.com/nim-works/nimskull/pull/28#discussion_r739863459
  proc vaWithConverter(args: varargs[int, convertToInt]): seq[int] = @args

  doAssert vaWithConverter(0.2) == @[0], "Arguments were converted to integers"
  doAssert used == 1, "Using specified varargs converter proc"


block immutable_arguments:
  # TODO IMPLEMENT
  discard


block mutable_arguments:
  # TODO IMPLEMENT
  discard

block passing_subtypes:
  ## It is possible to pass an object of derived type to the procedure expection parent type.
  ## Concrete details of the overload resolution and how correct procedure to call is selected
  ## are covered in the :idx:`t03_overload_core` section, here we only explain mecahnics of
  ## how the value is passed (and modified)
  ## Subtype match - argument is a subtype of the procedure argument. Note that this
  ## requirement is related to inheritance (because of subtyping), but does not
  ## replace `method`'s functionality. It also works with `{.inheritable.}` types and
  ## `object of RootObj` (in addition to `ref object of RootObj`) whereas `method`
  ## requires `ref` to be used.
  block inheritable_pragma:
    ## `{.inheritable.}` marks the object as non-final
    type
      Base {.inheritable.} = object
        fbase: int

      Derived = object of Base
        fderived: int

    block:
      proc aceptsImmutableBase(arg: Base) = discard

      aceptsImmutableBase(Derived())
      aceptsImmutableBase(Base())

    ## Note that only embedded supertype part is passed to the procedure in this
    ## case, and it is not safe to `cast` to a base type.
    ##
    ## ```
    ## Derived[Sup = Base[field1: int], field2: int]
    ##         ^^^^^^^^^^^^^^^^^^^^^^^
    ##         Only this part of the object is passed to the procedure.
    ## ```

    when not defined(vm) or defined(tryBrokenSpecification):
      block:
        # assignment to a var parameter is broken, fix should be in vmgen
        proc acceptsVarOfBase(arg: var Base) = arg = Base(fbase: 256)

        var derived = Derived(fderived: 128)
        ## It is possible to modify part of the embedded supertype via `var` argument
        acceptsVarOfBase(derived)
        doAssert derived.fbase == 256, "Assigned only to a section of a type"
        doAssert derived.fderived == 128, "Original field value is intact"

      block:
        proc acceptsPtrToBase(arg: ptr Base) = arg[] = Base(fbase: 12)

        var derived = Derived(fderived: 256)
        acceptsPtrToBase(addr derived)
        doAssert derived.fbase == 12, "Assigned only to a section of a type"
        doAssert derived.fderived == 256, "Original field value is intact"

      block:
        proc acceptsRefOfBase(arg: ref Base) = arg[] = Base(fbase: 12)
        var derived = (ref Derived)(fderived: 256)
        acceptsRefOfBase(derived)

        doAssert derived.fbase == 12, "Assigned only to a section of a type"
        doAssert derived.fderived == 256, "Original field value is intact"

## Current implementation of the argument passing for varargs has multiple
## inconsistencies and bugs. For more details see :idx:`t02_argument_passing_bugs`
## and :idx:`t03_overload_core_bugs`.