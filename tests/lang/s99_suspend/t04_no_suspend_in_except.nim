discard """
  description: "A 'suspend' must not be part of an 'except' clause."
  action: reject
"""

proc test() =
  try:
    discard
  except:
    suspend void, cont:
      return
