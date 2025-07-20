discard """
  description: "It's not allowed to suspend within a suspend block."
  action: reject
"""

proc test() =
  suspend void, cont:
    suspend void, cont:
      discard
