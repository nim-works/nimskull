discard """
  description: "Test for defer statements"
"""

block runs_at_the_end_of_scope:
  proc foo(): int =
    defer:
      inc result
    doAssert result == 0

  doAssert foo() == 1

block questionably_not_at_the_end_of_proc_scope:
  proc foo(): int =
    if true:
      defer:
        inc result
      doAssert result == 0
    doAssert result == 1
    inc result
    doAssert result == 2

  doAssert foo() == 2

block templates_do_not_create_scopes_defer_applies_to_the_surrounding:
  template bar() =
    defer:
      inc result

  proc foo(): int =
    bar()
    doAssert result == 0

  doAssert foo() == 1