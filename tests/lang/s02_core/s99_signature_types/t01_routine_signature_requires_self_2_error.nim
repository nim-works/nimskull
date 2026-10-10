discard """
  description: '''
    Every routine signature listed in a signature type must use the
    self type-variable parameter position at least once.
  '''
  action: reject
"""

type Sig = (signature(Self) do:
  # the self type-variable only being used in the return position is
  # not allowed
  proc p(x: int): Self
)
