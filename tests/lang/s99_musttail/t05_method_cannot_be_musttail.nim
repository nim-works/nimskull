discard """
  action: reject
"""

type Base = object of RootObj

method m(a: ref Base) {.musttail, base.} =
  discard
