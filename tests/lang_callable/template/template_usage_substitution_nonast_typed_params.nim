discard """
  description: '''
Templates parameters of non-AST type do not replace identifiers in new symbol
definition positions. Meaning a template parameter that is not `untyped` or
`typed` will not substitute for a matching identifier if defining things like
variables, routines, parameters, types, fields, etc.
'''
"""

block originally_this_did_not_work_now_it_does:
  # this was kept for historical reasons and can be replaced, when this was an
  # error it originated from https://github.com/nim-lang/nim/issues/3158
  type
    MyData = object
      x: int

  template newDataWindow(data: ref MyData): untyped =
    proc testProc(data: ref MyData): string =
      "Hello, " & $data.x
    testProc(data)

  var d: ref MyData
  new(d)
  d.x = 10
  doAssert newDataWindow(d) == "Hello, 10"