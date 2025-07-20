discard """
  description: "A 'suspend' must not be part of a 'finally' clause."
  action: reject
"""

proc test() =
  try:
    discard
  finally:
    suspend void, cont:
      return
