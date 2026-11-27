discard """
  description: "The 'self' type-variable may be modified by `sink`"
"""

type Sig = (signature(Self) do:
  proc identity(x: sink Self)
)
