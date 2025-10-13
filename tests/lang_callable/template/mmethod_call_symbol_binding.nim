proc p(x: int): int = x

template templ*(x: untyped): untyped =
  # `p` is a symbol that's not overloaded
  x.p()
