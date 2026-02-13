discard """
description: "Tests for the codegendecl pragma"
targets: "js"
"""

# codegendecl is only enabled for the js backend and is a noop for c

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