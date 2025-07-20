discard """
  description: "Borrows must not start before and end after a suspend."
  action: reject
"""

{.experimental: "views".}

proc test() =
  var i = 0
  var x: var int = i
  suspend void, cont:
    discard
  x = 2
  discard i # XXX: workaround for view-type shortcoming

test()
