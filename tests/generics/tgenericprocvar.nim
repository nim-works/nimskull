discard """
"""

# Test multiple generic instantiation of generic proc vars:

proc threadProcWrapper[TMsg](): string =
  var x: TMsg
  $x

#var x = threadProcWrapper[int]
#x()

#var y = threadProcWrapper[bool]
#y()

doAssert threadProcWrapper[int]() == "0"
doAssert threadProcWrapper[bool]() == "false"

type
  TFilterProc[T,D] = proc (item: T, env:D): bool {.nimcall.}

proc filter[T,D](data: seq[T], env:D, pred: TFilterProc[T,D]): seq[T] =
  result = @[]
  for e in data:
    if pred(e, env): result.add(e)

proc predTest(item: int, value: int): bool =
  return item <= value

proc test(data: seq[int], value: int): seq[int] =
  return filter(data, value, predTest)

var stuff: seq[string]
for x in items(test(@[1,2,3], 2)):
  stuff.add $x

doAssert stuff == @["1", "2"]
