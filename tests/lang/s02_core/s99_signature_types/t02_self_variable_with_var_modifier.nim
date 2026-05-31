discard """
  description: "The 'self' type-variable may be modified by `var`"
"""

type Sig = (signature(Self) do:
  proc identity(x: var Self)
)
