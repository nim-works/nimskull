discard """
description: '''
Objects subtype generics are bound using the most broad type and
not the most specific type
'''
knownIssue: "https://github.com/nim-lang/Nim/issues/18314"
"""

block:
  type
    A = ref object of RootObj
    B = ref object of A
    C = ref object of B


  block:
    proc impl(a: A): string = "A"
    proc impl(b: B): string = "B"

    doAssert impl(C()) == "B", "Subtype argument match"

  block:
    ## Substituting type names in generic constraints should
    ## work identically to the subtype match case, but instead
    ## broadest overload is selected.
    proc impl[T: A](a: T): string = "A"
    proc impl[T: B](b: T): string = "B"

    block:
      proc impl[T: C](c: T): string = "C"
      doAssert impl(C()) == "C", "Exact matching takes priority"

    doAssert impl(C()) == "B"
