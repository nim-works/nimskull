discard """
  description: "Methods cannot be coroutines"
  action: reject
"""

type
  Base = object of RootObj
  Sub = object of RootObj

method m(x: ref Object) {.coroutine.} =
  discard
