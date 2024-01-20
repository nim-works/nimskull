discard """
  output: '''i value 88
2aa'''
  knownIssue: "possible issue where result is not treated as var Concrete in passAround"
"""

import moverload_asgn2

proc passAround(i: int): Concrete =
  echo "i value ", i
  result = Concrete(a: "aa", b: "bb", rc: 0)

proc main =
  let
    i = 88
    v = passAround(i)
    z = v.a
  var
    x: Concrete
  x = v
  echo x.rc, z # 2aa

main()
