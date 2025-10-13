discard """
  description: '''
    The `sink` parameter modifier is not part of the parameter expression's
    effective type.
  '''
"""

proc test(param: sink int8) =
  # test let/var
  let a = param
  var b = param
  # no need to typeof-test `a` and `b`, since even if `a` and `b` were
  # inferred as `sink`-modified, `typeof` would drop the type

  # test aggregate/composite type introduction
  let tup = (param, param)
  let named = (a: param, b: param)
  let arr = [param]
  let se = {param}

  # test 'typeof'
  var x: (typeof(param),)

  doAssert $typeof(tup) == "(int8, int8)"
  doAssert $typeof(named) == "tuple[a: int8, b: int8]"
  doAssert $typeof(arr) == "array[0..0, int8]"
  doAssert $typeof(se) == "set[int8]"
  doAssert $typeof(x) == "(int8,)"

test(1)

# test parameter type inference from default value expression
proc param_test(x: sink int, y = x) =
  discard

doAssert $typeof(param_test) == "proc (x: sink int, y: int){.noSideEffect, gcsafe, locks: 0.}"
