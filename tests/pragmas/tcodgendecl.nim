discard """
description: "Tests for the codegendecl pragma"
targets: "c js"
"""

when defined(js):
  block regular:
    proc foo() {.codegendecl: "/* test */ function $2($3)".} =
      discard

    foo()

  block generic:
    # there was a regression with generics as the pragma was corrupted during
    # the initial pass
    proc foo[T]() {.codegendecl: "/* test */ function $2($3)".} =
      discard

    foo[int]()

else:
  # c backend

  block regular:
    proc foo() {.codegendecl: "/* test */ void $1($2)".} =
      discard

    foo()

  block generic:
    # there was a regression with generics as the pragma was corrupted during
    # the initial pass
    proc foo[T]() {.codegendecl: "/* test */ void $1($2)".} =
      discard

    foo[int]()