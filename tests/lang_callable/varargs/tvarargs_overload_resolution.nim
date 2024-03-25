discard """
  description: "Test varargs related overload resolution and precedence"
"""


block no_param_candidate_over_empty_varargs:
  discard "knownIssue: tvarargs_overload_resolution_known_issue_1"

block one_param_candidate_over_empty_varargs:
  proc foo(a: int): string = "one varargs param"
  proc foo(a: varargs[string], b: int): string = "empty varargs"

  doAssert foo(1) == "one varargs param"

block arity_has_greater_precedence_than_genericicity:
  block no_param_nullary_generic_candidate_over_empty_varargs:
    discard "knownIssue: tvarargs_overload_resolution_known_issue_2"

  block generic_one_param_candidate_over_empty_varargs:
    ## a generic match is superior to an empty varargs match, as the arity
    ## match is considered more precise
    proc foo[T](a: T): string = "no varargs param"
    proc foo(a: varargs[string], b: int): string = "empty varargs"

    doAssert foo(1) == "no varargs param"

  block generic_one_param_candidate_over_empty_varargs:
    ## a generic match is superior to an empty varargs match, as the arity
    ## match is considered more precise
    proc foo[T](a: T): string = "no varargs param"
    proc foo(a: varargs[int]): string = "varargs params"

    doAssert foo(1) == "no varargs param"

  block generic_varargs_match_loses_to_non_generic:
    ## with equivalent arity, the generic match loses to the concrete match
    func foo[T](a: varargs[T]): string = "generic"
    func foo(a: varargs[int]): string = "non-generic"

    doAssert foo(1) == "non-generic"

block seq_param_candidate_matches_over_varargs:
  proc foo(a: seq[int]): string = "seq"
  proc foo(a: varargs[int]): string = "varargs"

  doAssert foo(1) == "varargs"
  doAssert foo(1, 2) == "varargs", "should not match seq"
  doAssert foo(@[1]) == "seq"
  doAssert foo(@[1, 2]) == "seq"

block conversion_vs_subtype_precedence:
  block subtype_precedes_conversion:
    type Foo = range[1..3]
    proc fooMaker(i: int): Foo =
      case i
      of 1: Foo 1
      of 2: Foo 2
      of 3: Foo 3
      else: Foo 3

    proc foo(a: varargs[int32]): string = "int"
    proc foo(a: varargs[Foo, `fooMaker`]): string = "conv"

    doAssert foo(1, 2, 3) == "int", "int literals are a subtype of int32"
    doAssert foo(Foo 1, Foo 2, Foo 3) == "conv", "exact match wins"

  block subtype_widening_illegal_conversion_precedes:
    proc toInt32(i: int8): int32 = int32 i
    proc toInt32(i: int16): int32 = int32 i

    proc foo(a: varargs[int16]): string = "int16"
    proc foo(a: varargs[int32, toInt32]): string = "conv"

    doAssert foo(1'i16, 2'i8) == "conv"

block value_inheritance_precedes_conversion:
  discard "if conversions don't differentiate signatures, then other rules will handle this"

block ref_inheritance_precedes_conversion:
  discard "if conversions don't differentiate signatures, then other rules will handle this"


type
  Foo = object
          a: int

var converterCalls = 0
converter toFoo(i: int): Foo =
  inc converterCalls
  Foo(a: i)

block conversion_precedes_converters:
  var makeFooCalls = 0
  proc makeFoo(i: int): Foo = inc makeFooCalls; Foo(a: i)
  proc foo(v: varargs[Foo, makeFoo]) = discard

  foo(1, 2, 3)
  doAssert converterCalls == 0, "the converter shouldn't have been used"
  doAssert makeFooCalls == 3, "conversion call per arg passed"