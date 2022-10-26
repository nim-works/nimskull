discard """
  action: compile
"""

type
  A* = object
    v: int

func get(r: var A): var int =
  r.v

func p(x: sink A): int =
  result = move(get x) # internal assertion triggered here

static:
  proc pr() =
    # It's important that the `x` is not a global here, since no sink would be
    # performed otherwise
    var x: A
    let y = p(x)

  pr()