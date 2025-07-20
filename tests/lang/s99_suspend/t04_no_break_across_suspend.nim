discard """
  description: "A 'break' cannot cross a suspend block boundary."
  action: reject
"""

proc test() =
  block L1:
    suspend void, cont:
      break L1
