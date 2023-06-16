discard """
  knownIssue: '''
    variables defined in statement list that are later lifted into lambda
    expressions are first semantically analysed as normal module-level code,
    causing them to stay as globals, which is invalid and confuses the
    compiler backend
  '''
"""

# https://github.com/nim-lang/nim/issues/7104

var output: seq[int]

proc sp(cb: proc())=
  cb()

sp:
  var i = 0 # <- this is currently treated as a global owned by the module
  output.add(i)
  sp():
    inc i
    output.add(i)
    sp do:
      inc i
      output.add(i)

doAssert not compiles(i == 3)
doAssert output == [0, 1, 2]