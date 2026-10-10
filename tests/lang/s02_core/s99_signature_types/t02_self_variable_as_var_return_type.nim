discard """
  description: '''
    The 'self' type-variable may appear immediately in the return type
    position, modified with `var`.
  '''
"""

type Sig = (signature(Self) do:
  proc identity(x: var Self): var Self
)
