discard """
  description: '''
    Routine declarations with the same names and the same parameter types are
    not allowed.
  '''
  action: reject
"""

type Sig = (signature(Self) do:
  proc p(x: Self)
  proc p(x: Self)
)
