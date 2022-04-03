discard """
  targets: "c cpp"
  output: '''(a: "a", b: "b", c: "")
caught
(a: "a", b: "b", c: "")'''
"""

# bug #14126

type X = object
  a, b, c: string

proc f(): X =
  result.a = "a"
  result.b = "b"
  raise (ref ValueError)()

proc ohmanNoNRVO =
  var x: X
  x.a = "1"
  x.b = "2"
  x.c = "3"

  try:
    x = f()
  except:
    discard

  echo x
  # once NVRO is sorted out, x.c == "3"
  doAssert x.c == "", "shouldn't modify x if f raises"

ohmanNoNRVO()

proc ohmanNoNRVO2(x: var X) =
  x.a = "1"
  x.c = "3"
  x = f()

var xgg: X
try:
  ohmanNoNRVO2(xgg)
except:
  echo "caught"
echo xgg
# once NVRO is sorted out, xgg.c == "3"
doAssert xgg.c == "", "this assert will fail"
