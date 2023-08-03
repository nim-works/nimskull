discard """
  targets: "c !js vm"
  description: '''
    Regression test for a double-evaluation bug involving assignment to or
    creating view of string characters
  '''
"""

# knownIssue: double evaluation bug with arguments to ``swap`` when using
# the JavaScript backend

block:
  # for a string access used as the argument to a ``swap`` call, the string
  # expression was evaluated twice. The string access had to be string
  # subscript expression *directly*; a statement list expression didn't
  # trigger the code generator bug
  var i = 0
  var a = "abc"
  var b = "def"

  # still test with a statement list expression:
  swap((inc i; a[0]), (inc i; b[0]))
  doAssert a == "dbc" and b == "aef"
  doAssert i == 2, $i

  proc get(x: var string): var string =
    inc i # <- side effect
    x

  i = 0
  # the bug was triggered here:
  swap(get(a)[0], get(b)[0])
  doAssert a == "abc" and b == "def"
  doAssert i == 2

block:
  # assigning the result of a string subscript expression to a ``var char``
  # location (i.e., creating a view) also triggered the bug
  var i = 0
  proc get(x: var string): var string =
    inc i
    x

  proc access(s: var string): var char =
    result = get(s)[0]

  var s = "abc"
  access(s) = 'd'
  doAssert s == "dbc"
  doAssert i == 1, $i