discard """
description: '''
  Test for call AST corruption where typing an argument which fails, and then
  is reused in an untyped context results in losing the original untyped AST.
'''
"""

# This test was originally found via `sequtils.toSeq`, hence the extract from
# that part of the standard library is used below.

proc toSeqBuiltin[T](arg: cstring): seq[T] {.inline.} =
  let len = arg.len
  result.newSeq(len)
  var i = 0
  while i < len:
    result[i] = arg[i]
    inc i

template toSeqBuiltin[T](arg: untyped): untyped =
  var tmp: seq[T] = @[]
  for it in arg:
    tmp.add it
  tmp

template toSeq*(arg: untyped): untyped =
  mixin items
  when compiles(typeof(items(arg))):
    # arg is something that supports the ``items`` iterator:
    toSeqBuiltin[typeof(items(arg))](arg)
  elif compiles(typeof(arg())):
    # arg must be the symbol of an iterator:
    toSeqBuiltin[typeof(arg())](arg())
  else:
    # arg must be an iterator invocation expression itself:
    toSeqBuiltin[typeof(arg)](arg)

iterator match[T](x: seq[T], v: T): T =
  for item in x:
    if item == v:
      yield v

proc match[T](x: seq[T], v: T): bool =
  v in x

proc main() =
  let x = @[1, 1, 2, 3]
  echo toSeq x.match(1)

main()