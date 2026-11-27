discard """
  description: '''
    The 'self' type-variable may appear immediately in the return type
    position.
  '''
"""

type Sig = (signature(Self) do:
  proc identity(x: Self): Self
)
