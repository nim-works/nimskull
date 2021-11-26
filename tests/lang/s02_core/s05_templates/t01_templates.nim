block template_call:
  template impl() =
    values.add "in template"

  ## Note how `values` variable is defined *after* the template body -
  ## because template simply performs code substitution it can use
  ## variables defiend *after* template itself.
  var values: seq[string]
  values.add "before"
  impl()
  values.add "after"

  doAssert values == @[
    "before",
    "in template",
    "after"
  ]

block template_as_expression:
  template expr(): untyped = 12

  doAssert expr() == 12


block template_arguments:
  template templ(arg: int): untyped = arg

  doAssert templ(12) == 12

block note_mutliple_evaluation_bug:
  ## Because template simply substitutes an code in place of it's
  ## arguments, any expression will be evaluated *each time it is
  ## encoutered in the body*, which might lead to the so-called "double
  ## evaluation bug". It exists because of the very nature of the templates
  ## and macros can hardly be detected automatically.
  var value = 0
  proc get(): int =
    inc value
    return value

  template templ(arg: untyped): untyped = arg + arg

  doAssert templ(get()) == (0 + 1) + (0 + 1 + 1)

block identifier_join:
  ## It is possible to construct new identifiers in the template body using
  ## backtick notation.

  template getVar(idx: untyped): untyped = `varName idx`

  var
    varNameQq = 30
    varNameZzz = 50

    varName1 = 12
    varName2 = 20


  when defined(tryBrokenSpecification):
    ## Stropping does not work with identifier join in templates
    var `varName??` = 40
    doAssert getVar(`??`) == 40

  doAssert getVar(Qq) == 30

  ## Identifier joining is also style-insensetive - variable `varNameZzz`
  ## can be accessed by passing `zzz`.
  doAssert getVar(zzz) == 50

  ## Integer literals can be used as well
  doAssert getVar(1) == 12
  doAssert getvar(2) == 20


  ## Note that template arguments are substituted as a code, not by value.
  ## That's why calling `getVar(idx)` with `idx` being a variable or even a
  ## constant (which are known at compile-time), will result in the
  ## `varNameIdx` variable being accessed.

  var varNameIdx = 900

  block:
    let idx = 1
    doAssert getVar(idx) == 900

  block:
    const idx = 1
    doAssert getVar(idx) == 900
