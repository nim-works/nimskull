discard """
  description: '''
    Using the 'self' type-variable in any position that's not an immediate
    parameter position is not allowed.
  '''
  action: reject
"""

type Sig = (signature(Self) do:
  proc p(x: seq[Self])
)
