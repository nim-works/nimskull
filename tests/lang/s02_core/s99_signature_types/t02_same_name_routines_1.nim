discard """
  description: '''
    Multiple routine declarations in a signature may share the same name, as
    long as their signatures are different.
  '''
"""

type Sig = (signature(Self) do:
  proc p(x: Self, y: int)
  proc p(x: Self, y: float)
)
