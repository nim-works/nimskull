discard """
  targets: "c js vm"
  description: "Tests for indirectly modifying a string's element"
"""

# indirect here means: not via a direct assignment where the left-hand side
# is a string subscript operation

{.experimental: "views".}

block through_view:
  var
    s = "abc"
    v: var char = s[0]

  v = 'd'
  doAssert s == "dbc"

block through_var_parameter:
  var s = "abc"
  proc p(c: var char) =
    c = 'd'

  p(s[0])
  doAssert s == "dbc"

block through_swap:
  var
    s = "abc"
    c = 'd'

  swap(s[0], c)

  doAssert c == 'a'
  doAssert s == "dbc"

block through_var_openarray:
  var s = "abc"
  proc p(v: var openArray[char]) =
    v[0] = 'd'

  p(s)
  doAssert s == "dbc"