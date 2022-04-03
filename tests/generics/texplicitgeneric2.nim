discard """
"""

# test explicit type instantiation

type
  TDict*[TKey, TValue] = object
    data: seq[tuple[k: TKey, v: TValue]]
  PDict*[TKey, TValue] = ref TDict[TKey, TValue]

proc newDict*[TKey, TValue](): PDict[TKey, TValue] =
  new(result)
  result.data = @[]

proc add*(d: PDict, k: d.TKey, v: d.TValue) =
  d.data.add((k, v))


iterator items*(d: PDict): tuple[k: d.TKey, v: d.TValue] =
  for k, v in items(d.data): yield (k, v)

block test_1:
  var
    actual = ""
    d = newDict[int, string]()
  d.add(12, "12")
  d.add(13, "13")
  for k, v in items(d):
    actual.add "Key: " & $k & " value: " & v
  doAssert actual == "Key: 12 value: 12Key: 13 value: 13"

block test_2:
  var
    actual = ""
    c = newDict[char, string]()
  c.add('A', "12")
  c.add('B', "13")
  for k, v in items(c):
    actual.add "Key: " & $k & " value: " & v
  doAssert actual == "Key: A value: 12Key: B value: 13"
