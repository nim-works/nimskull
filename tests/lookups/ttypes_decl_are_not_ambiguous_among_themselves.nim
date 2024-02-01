discard """
  description: '''
    Ensure types are not considered ambiguous among type declarations,
    allowing for parameter definitions that repeat the same type, such as:
    `f(v: V, w: V) = discard`
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
