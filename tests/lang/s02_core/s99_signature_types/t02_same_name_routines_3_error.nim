discard """
  description: "Parameter names are not considered for declaration equality"
  action: reject
"""

type Sig = (signature(Self) do:
  proc p(a: Self)
  proc p(b: Self)
  # the declarations use different parameters names, yet are still equal
)
