discard """
  description: "It's not allowed to suspend an iterator."
  action: reject
"""

iterator test(): int =
  suspend void, cont:
    discard
