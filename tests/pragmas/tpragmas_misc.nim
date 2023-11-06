discard """
description: "test macros pragmas on templates"
"""

##[
tests for misc pragmas that don't need a separate file
]##

block can_undefine_non_existent_define:
  # this should work as it's idempotent
  static: doAssert not defined(tpragmas_misc_def)
  {.undef(tpragmas_misc_def).} # works even if not set

block can_define_a_fresh_define:
  static: doAssert not defined(tpragmas_misc_def)
  {.define(tpragmas_misc_def).}

block undef_removes_an_existing_define:
  static: doAssert defined(tpragmas_misc_def)
  {.undef(tpragmas_misc_def).}
  static: doAssert not defined(tpragmas_misc_def)

block: # (partial fix) bug https://github.com/nim-lang/nim/issues/15920
  block: # var template pragmas don't work in templates
    template foo(expr) =
      expr
    
    proc fun1()=
      let a {.foo.} = 1
    
    template fun2()=
      let a {.foo.} = 1
    
    fun1() # ok
    fun2() # also works

  template foo2() = discard # distractor (template or other symbol kind)
  block:
    template foo2(expr) =
      expr

    proc fun1()=
      let a {.foo2.} = 1

    template fun2()=
      let a {.foo2.} = 1

    fun1() # ok
    fun2() # also works

  block template_pragmas_for_templates:
    # adapted from $nim/lib/std/private/since.nim

    # case without overload
    template since3(version: (int, int), body: untyped) {.dirty.} =
      when (NimMajor, NimMinor) >= version:
        body
    
    template fun3(): int {.since3: (1, 3).} = 12

  block template_pragmas_for_templates_with_overloads:
    # case with overload
    template since2(version: (int, int), body: untyped) {.dirty.} =
      when (NimMajor, NimMinor) >= version:
        body

    template since2(version: (int, int, int), body: untyped) {.dirty.} =
      when (NimMajor, NimMinor, NimPatch) >= version:
        body

    template fun3(): int {.since2: (1, 3).} = 12

from macros import genSym
block:
  template fn() =
    var ret {.gensym.}: int # special case template pragmas so it doesn't get confused
    discard ret
  fn()
  static: discard genSym()

block:
  macro foo(x): untyped = x
  template bar {.pragma.}

  proc a {.bar.} = discard # works
  proc b {.bar, foo.} = discard # doesn't

block recover_from_wrong_accent_quote_with_template:
  # ensure that template pragma can recover from an accented quote that
  # fails to evaluate
  template drop(x: untyped): untyped =
    # transforms the applied-to-definition into a discard statement
    discard

  # XXX: a dirty template is currently required for the accent quote to
  #      use the parameter
  template templ(a: untyped) {.dirty.} =
    # the accent-quote being illformed must not cause an error, as
    # the template transforms the whole definition away
    proc p() {.`a`, drop.} =
      discard

  # pass something to the template that can't be used as part of an accented
  # quote
  templ(call())

block recover_from_wrong_accent_quote_with_macro:
  # same as the above test, but using a macro
  macro drop(x: untyped): untyped =
    discard

  template templ(a: untyped) {.dirty.} =
    proc p() {.`a`, drop.} =
      discard

  templ(call())