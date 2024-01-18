discard """
  targets: "c js vm"
  knownIssue: '''
    Expressions part of compound expressions are not re-typed during fitting,
    leading to the empty container types not being fixed-up and thus
    ``tyEmpty`` reaching into the code generator
  '''
  description: '''
    Ensure that compound expressions of empty container type are
    typed properly once a concrete, receiving type is known
  '''
"""

proc get(x: array[0, int]): array[0, int] = x
proc get(x: seq[int]): seq[int] = x
proc get(x: set[char]): set[char] = x

var cond = false # value doesn't matter

# note: nil expressions are not included here since they're ambiguous
# with nil statements

# if expressions:
discard get(if cond:  [] else:    [])
discard get(if cond: @[] else:   @[])
discard get(if cond:  {} else:    {})

# case expressions:
discard get:
  case cond
  of true:  []
  of false: []
discard get:
  case cond
  of true:  @[]
  of false: @[]
discard get:
  case cond
  of true:  {}
  of false: {}

# block expressions:
discard get(block:  [])
discard get(block: @[])
discard get(block:  {})

# try expressions:
discard get:
  try:     []
  except:  []
  finally: discard
discard get:
  try:     @[]
  except:  @[]
  finally: discard
discard get:
  try:     {}
  except:  {}
  finally: discard