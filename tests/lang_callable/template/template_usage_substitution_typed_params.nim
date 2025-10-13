discard """
  description: '''
Except for `untyped`, template parameters do not replace identifiers in new
symbol definition positions. Meaning a template parameter that is not `untyped`
will not substitute for a matching identifier if defining things like
variables, routines, parameters, types, fields, etc.
'''
"""

block originally_this_did_not_work_now_it_does:
  # this was kept for historical reasons and can be replaced, when this was an
  # error, example originated from https://github.com/nim-lang/nim/issues/3158
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

block also_true_for_typed_parameters:
  template foo(a: typed): untyped =
    let a = a + 10
    a
  let x = foo(10)
  doAssert x == 20