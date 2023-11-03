discard """
  targets: "c js vm"
  description: '''
    Ensure that expressions of empty container and ``typeof(nil)`` type are
    typed properly once a concrete, receiving type is known
  '''
"""

proc get(x: pointer): pointer = x
proc get(x: array[0, int]): array[0, int] = x
proc get(x: seq[int]): seq[int] = x
proc get(x: set[char]): set[char] = x

proc get2(x: openArray[int]): int =
  # test with an ``openArray`` receiver type. Since without view-types an
  # openArray cannot be returned, return the length to make sure that the
  # parameter was passed correctly
  x.len

# simple case: empty-container typed expression is passed directly
discard get(nil)
discard get([])
discard get(@[])
discard get({})

doAssert get2([]) == 0
doAssert get2(@[]) == 0

# more complex case: statement-list expressions
discard get((discard; nil))
# XXX: not working yet
#discard get((discard; []))
discard get((discard; @[]))
discard get((discard; {}))

doAssert get2((discard; [])) == 0
doAssert get2((discard; @[])) == 0