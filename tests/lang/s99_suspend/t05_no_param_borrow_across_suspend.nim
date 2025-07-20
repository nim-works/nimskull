discard """
  description: "Non-owning parameters must not be used beyond a 'suspend'."
  action: reject
"""

proc test(i: int) =
  suspend void, cont:
    discard
  let x = i

test(1)
