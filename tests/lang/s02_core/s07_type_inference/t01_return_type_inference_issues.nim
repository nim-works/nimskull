discard """
  knownIssue: '''
    Standalone composite type-classes used as return types always result in
    an error
  '''
"""

type Generic[T] = (T, T)

# currently fails with: "cannot instantiate"

proc p3(): Generic =
  result = ("", "")

doAssert typeof(p3()).T is string

iterator iter3(): Generic =
  yield ("", "")

doAssert typeof(iter3()).T is string
