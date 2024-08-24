discard """
  targets: "c js vm"
  description: '''
    Ensure that expressions of empty container and ``typeof(nil)`` type are
    typed properly once a concrete, receiving type is known
  '''
"""

type Generic[T] = seq[T]

proc get(x: pointer): pointer = x
proc get(x: array[0, int]): array[0, int] = x
proc get(x: seq[int]): seq[int] = x
proc get(x: set[char]): set[char] = x

proc get2(x: openArray[int]): int =
  # test with an ``openArray`` receiver type. Since without view-types an
  # openArray cannot be returned, return the length to make sure that the
  # parameter was passed correctly
  x.len

proc get3(x: Generic[int]): Generic[int] =
  # test with a generic receiver type
  x

# simple case: empty-container typed expression is passed directly
discard get(nil)
discard get([])
discard get(@[])
discard get({})

doAssert get2([]) == 0
doAssert get2(@[]) == 0

discard get3(@[])

# more complex case: statement-list expressions
discard get((discard; nil))
# XXX: not working yet
#discard get((discard; []))
discard get((discard; @[]))
discard get((discard; {}))

doAssert get2((discard; [])) == 0
doAssert get2((discard; @[])) == 0

discard get3((discard; @[]))

# -----------------
# static parameters

proc get_static(x: static pointer): pointer = x
proc get_static(x: static array[0, int]): array[0, int] = x
proc get_static(x: static seq[int]): seq[int] = x
proc get_static(x: static set[char]): set[char] = x

# simple case: empty-container typed expression is passed directly
discard get_static(nil)
discard get_static([])
discard get_static(@[])
discard get_static({})

# more complex case: statement-list expressions
discard get_static((discard; nil))
# XXX: not working yet
#discard get_static((discard; []))
discard get_static((discard; @[]))
discard get_static((discard; {}))
