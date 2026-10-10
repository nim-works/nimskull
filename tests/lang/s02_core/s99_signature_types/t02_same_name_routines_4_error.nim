discard """
  description: '''
    Routine signatures for same-named declarations differing in only the
    return type are not allowed.
  '''
  action: reject
"""

type Sig = (signature(Self) do:
  proc p(x: Self): int
  proc p(x: Self)
)
