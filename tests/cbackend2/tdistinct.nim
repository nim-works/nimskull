
block:
  ## converting an lvalue of type ``distinct T`` to ``T`` must yield an lvalue
  ## with the same identity
  type DInt = distinct int

  proc a(x: var int) =
    x = 1

  var v = DInt(0)
  a(v.int)
  doAssert v.int == 1