discard """
  description: '''
    Ensure that a type expression can be used in the type slot of an object
    construction syntax, which is itself the argument to a `[]` procedure
    call
  '''
"""

# this is a regression test for an AST corruption

type O = object

# minimized version of the bug:
discard `[]`((ref O)())

# currently equivalent version of the issue, where a template is used:
template templ() =
  discard (ref O)()[]

templ()