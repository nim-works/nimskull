discard """
  description: "The specified type must be a concrete type."
  action: reject
"""

type Generic[T] = object

proc test() =
  suspend Generic, cont:
    discard
