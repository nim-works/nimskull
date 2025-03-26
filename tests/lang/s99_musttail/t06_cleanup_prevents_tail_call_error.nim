discard """
  action: reject
"""

type Data = object

proc `=copy`(a: var Data, b: Data) =
  a = b

proc p() {.musttail.} = discard

proc test() {.musttail.} =
  var x = Data()
  # x must be implicitly destroyed on scope exit, preventing the call from
  # being a tail call
  p()

test()
