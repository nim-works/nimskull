discard """
  output: '''
n
n
(one: 1, two: 2, three: 3)
1
2
3
(one: 4, two: 5, three: 6)
4
(one: 7, two: 8, three: 9)
7
8
9
'''
"""


block tindex:
  type
    TMyTuple = tuple[a, b: int]

  proc indexOf(t: typedesc, name: string): int =
    ## takes a tuple and looks for the field by name.
    ## returs index of that field.
    var
      d: t
      i = 0
    for n, x in fieldPairs(d):
      if n == name: return i
      i.inc
    raise newException(ValueError, "No field " & name & " in type " &
      astToStr(t))

  doAssert TMyTuple.indexOf("b") == 1



block ttemplate:
  # bug #1902
  # This works.
  for name, value in (n: "v").fieldPairs:
    echo name

  template wrapper(): void =
    for name, value in (n: "v").fieldPairs:
      echo name
  wrapper()



block tbreak:
  # bug #2134
  type
    TestType = object
      one: int
      two: int
      three: int

  var
    ab = TestType(one:1, two:2, three:3)
    ac = TestType(one:4, two:5, three:6)
    ad = TestType(one:7, two:8, three:9)
    tstSeq = [ab, ac, ad]

  for tstElement in mitems(tstSeq):
    echo tstElement
    for tstField in fields(tstElement):
      #for tstField in [1,2,4,6]:
      echo tstField
      if tstField == 4:
        break

