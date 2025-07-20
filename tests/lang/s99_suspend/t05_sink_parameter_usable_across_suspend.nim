discard """
  description: "Owned parameters may be used beyond a 'suspend'."
"""

proc test(i: sink int): int =
  suspend void, cont:
    cont[1](cont[0])
  result = i

doAssert test(10) == 10
