discard """
  description: '''
    Specification for overloading with parametric polymorphism/generics
  '''
"""

block most_specific_generic_wins:
  ## If multiple overloaded procedures are present, most specific is
  ## always selected.

  block qualifier:
    ## `ref`, `ptr` and other type qualifiers are counted as a part of
    ## the generic procedure definition.

    proc gen[T](x: var ref ref T): string = "var ref ref T"
    proc gen[T](x: ref ref T): string = "ref ref T"
    proc gen[T](x: ref T): string = "ref T"
    proc gen[T](x: T): string = "T"

    var v: ref ref int = nil
    doAssert gen(v) == "var ref ref T"
    doAssert gen((ref ref int)(nil)) == "ref ref T"
    doAssert gen((ref int)(nil)) == "ref T"
    doAssert gen(nil) == "T"


  # QUESTION - it is not possible to select less generic overload?
  # doAssert gen[ref ref ref ref int](nil) == "ref T"

  block regular:
    ## The rule also applied to regular generic types
    proc gen[T](x: var seq[seq[T]]): string = "var seq seq T"
    proc gen[T](x: seq[seq[T]]): string = "seq seq T"
    proc gen[T](x: seq[T]): string = "seq T"
    proc gen[T](x: T): string = "T"

    var v: seq[seq[int]]
    doAssert gen(v) == "var seq seq T"
    doAssert gen(newSeq[seq[int]]()) == "seq seq T"
    doAssert gen(newSeq[int]()) == "seq T"
    doAssert gen(nil) == "T"

block generic_vs_subtype:
  ## Generic overloading match comes before subtype relation match, so maybe code
  ## like this would seem unexpected.
  #
  # Example taken from the https://github.com/nim-lang/Nim/issues/18314
  #
  type
    A = ref object of RootObj
    B = ref object of A
    C = ref object of B

  block regular_procs:
    proc impl[T: A](a: T): string = "matched generic"
    proc impl(b: B): string = "matched subtype B"

    ## subtype vs generic - wins generic
    doAssert impl(C()) == "matched generic"

    ## generic vs exact match - wins exact match
    doAssert impl(B()) == "matched subtype B"

block subtype_constaints_for_generic:
  ## When subtype is used as a constraint for the generic parameter it
  ## functions the same way as if it was written directly in the argument.

  type
    A  = ref object of RootObj
    B1 = ref object of A
    B2 = ref object of A

  proc impl[T](a: T): string = "T"
  proc impl[T: A](a: T): string = "A"
  proc impl[T: B1](a: T): string = "B1"
  proc impl[T: B2](a: T): string = "B2"
  proc impl[T: B1 | B2](a: T): string = "B1|B2"

  doAssert impl(B1()) == "B1"
  doAssert impl(B2()) == "B2"

  ## Note that `B1 | B2` is not reported as an ambiguous overload since constaints
  ## with alternatives have a lower precedence compared to the simple `SomeType`.
