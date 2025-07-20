discard """
  description: "It's not allowed to suspend in a static/const context."
  action: reject
"""

proc test() =
  static:
    suspend void, cont:
      discard
