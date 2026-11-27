discard """
  description: '''
    Every routine signature listed in a signature type must use the
    self type-variable in a parameter position at least once.
  '''
  action: reject
"""

type Sig = (signature(Self) do:
  proc p(x: int)
)
