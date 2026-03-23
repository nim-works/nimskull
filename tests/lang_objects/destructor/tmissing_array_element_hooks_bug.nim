discard """
  description: '''
    Regression test for a compiler crash in the edge case where the tuple type
    used as an array element type is not used anywhere else and a temporary of
    said tuple type has to be materialized.
  '''
  joinable: false
"""

type Object = object

proc `=destroy`(x: var Object) =
  discard

proc test(): Object = discard

# how the second element expression looks like is irrelevant as long as
# it conditionally raises
var arr = [(Object(),), (test(),)]
