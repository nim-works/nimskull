discard """
  description: '''
    Ensure types are preferred in type contexts when the name is overloaded
    with parameter and/or generic parameter names.
  '''
"""

# This is a regression test for an infinite loop in `semTypeIdent`
# see: https://github.com/nim-works/nimskull/issues/1151


type
  FOO = object

block parameter_and_return_type_repetition:
  # the parameter name is kept as `Foo` to force an ambiguitity between `Foo`
  # parameter name, and `FOO` the parameter type, as that's what resulted in
  # an infinite loop.
  proc foo(Foo: FOO): FOO = discard

block parameter_and_parameter_repetition:
  proc foo(Foo: FOO, bar: FOO) = discard

block type_and_generic_parameter_collison:
  # xxx: this is questionable, it should probably be an ambiguity error, as
  #      `FOO` the generic parameter is more 'local' but type names on a per
  #      module basis should be unambiguous, at least in principal.
  proc foo[FOO](foo: FOO): FOO = discard
  doAssert foo[int](0) is int