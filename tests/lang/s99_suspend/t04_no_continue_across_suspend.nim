discard """
  description: "A 'continue' cannot cross a suspend block boundary."
  action: reject
"""

proc test() =
  while true:
    suspend void, cont:
      continue
