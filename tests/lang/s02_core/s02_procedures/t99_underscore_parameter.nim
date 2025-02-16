discard """
  description: '''
    Multiple parameters can use an underscore as the name, making the
    parameters inaccessible
  '''
"""

proc test1(_: int) = discard
test1(1)

proc test2(_: int, _: int) = discard
test2(1, 2)

proc mixed(a: int, _: int) = discard
mixed(1, 2)

## The same is true for parameters of anonymous procedures.
let anon = proc(_, _, _: int) = discard
anon(1, 2, 3)
