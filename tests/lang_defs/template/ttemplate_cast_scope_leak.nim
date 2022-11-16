discard """
  targets: "c cpp"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/9534
      symbol used in casting a proc leaks into outer scope
    . In the following example the symbol o used in the cast
      `cast[proc (o: int): int {.nimcall.}](p)` seems to leak into
      the outer scope
    . The issue disappears when using a different identifier in the cast
      than o. Also, the problem happens only with the template.
    '''
"""

type
  Object = object
    data: int

template test() =
  proc methodName(o: Object): int =
    var p: pointer
    doAssert o.data == 521
    let f {.used.} = cast[proc (o: int): int {.nimcall.}](p)
    doAssert o.data == 521
    result = 1314

  var a = Object(data: 521)
  doAssert methodName(a) == 1314

test()


