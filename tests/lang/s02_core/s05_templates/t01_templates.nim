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

  block using_an_identifier:
    var varNameQq = 30
    doAssert getVar(Qq) == 30

  block is_style_insensitive:
    ## Identifier joining is also style-insensitive - variable `varNameZzz`
    ## can be accessed by passing `zzz`.
    var varNameZzz = 50
    doAssert getVar(zzz) == 50

  block works_with_string_literals:
    var varNameFoo = 12
    doAssert getVar("foo") == 12
    doAssert getVar("""foo""") == 12
    doAssert getVar(r"foo") == 12

  block works_with_character_literals:
    var varNameB = 20
    doAssert getVar("b") == 20

  block works_with_bool_literals:
    ## bool literals can be used as well
    var varNameFalse = 30
    doAssert getVar(false) == 30

  block works_with_unsigned_integer_literals:
    var varName1 = 40
    doAssert getVar(1'u)   == 40
    doAssert getVar(1'u8)  == 40
    doAssert getVar(1'u16) == 40
    doAssert getVar(1'u32) == 40
    doAssert getVar(1'u64) == 40

  block works_with_integer_literals:
    var varName2 = 50
    doAssert getVar(2)     == 50
    doAssert getvar(2'i8)  == 50
    doAssert getvar(2'i16) == 50
    doAssert getvar(2'i32) == 50
    doAssert getvar(2'i64) == 50

    when false:
      # These are gaps in the spec, what sort of error should result?
      # the spec feels really half-baked at this point... we should probably
      # create an identifier type and constructor and only accept those
      getVar(-2)   # produces "varName-2"
      getVar('\n') # produces "varName10" ... wtf
      getVar("\n") # produces "varName<newLine>"

  when defined(tryBrokenSpecification):
    block works_with_stropping:
      ## Stropping does not work with identifier join in templates
      var `varName??` = 50
      doAssert getVar(`??`) == 50

  # TODO: these aren't really identifier construction tests, move them out
  block substitution_is_ast_based:
    ## Note that template arguments are substituted as a code, not by value.
    ## That's why calling `getVar(idx)` with `idx` being a variable, will
    ## result in the `varNameIdx` variable being accessed.
    block subsitute_ast_as_is:
      var varNameIdx = 900
      let idx {.used.} = 1
      doAssert getVar(idx) == 900

    block same_with_const_known_at_compile_time:
      var varNameIdx = 910
      const idx {.used.} = 1
      doAssert getVar(idx) == 910

    block typed_will_take_the_value_of_a_const_as_we_eval_it_first:
      template getVarTyped(idx: typed): untyped = `varName idx`
      var varName90 = 1
      const foo = 90
      doAssert getVarTyped(foo) == 1

    when defined(tryBrokenSpecification):
      # for some reason the value "bar" isn't being passed unlike the int above
      block typed_will_take_the_value_of_a_const_as_we_eval_it_first:
        template getVarTyped(idx: typed): untyped = `varName idx`
        var varNameBar = 1
        const foo = "bar"
        doAssert getVarTyped(foo) == 1