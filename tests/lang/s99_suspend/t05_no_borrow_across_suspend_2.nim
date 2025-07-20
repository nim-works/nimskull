discard """
  description: "Borrows must not start before and end after a suspend."
  action: reject
"""

{.experimental: "views".}

proc test() =
  var s = @[1, 2, 3]
  var x: openArray[int] = s
  suspend void, cont:
    discard
  x[0] = 1
  discard s # XXX: workaround for a view shortcoming

test()
