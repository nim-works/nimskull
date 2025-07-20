discard """
  description: "A 'suspend' must not appear within a 'defer' block."
  action: reject
"""

proc test() =
  defer:
    suspend void, cont:
      discard
