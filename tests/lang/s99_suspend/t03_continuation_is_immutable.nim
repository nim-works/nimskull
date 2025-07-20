discard """
  description: "The local storing the continuation instance is immutable."
  action: reject
"""

proc test() =
  suspend void, cont:
    cont = default(typeof(cont))
