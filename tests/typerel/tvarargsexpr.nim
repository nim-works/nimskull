discard """
  output: '''success'''
"""

import std/macros

block varargs_untyped_arg_list_construction_no_array_flattening:
  #bug https://github.com/nim-lang/Nim/issues/913

  macro thirteen(args: varargs[untyped]): int =
    doAssert args.kind == nnkArgList
    if args.len == 1:
      # this is when we pass in a single `[1, 2]`
      doAssert args[0].kind == nnkBracket
      doAssert args[0].len == 2
      doAssert args[0][0].intVal == 1
      doAssert args[0][1].intVal == 2
    result = newIntLitNode(13)

  doAssert(13==thirteen([1,2])) # works, and doesn't flatten the array
  doAssert(13==thirteen(1,2)) # works

  doAssert(13==thirteen(1,[2])) # wasn't working
  doAssert(13==thirteen([1], 2)) # wasn't working

block varargs_untyped_act_as_rest_param:
  # overrides named parameter matching
  # bug https://github.com/nim-lang/Nim/issues/2545

  macro test(e: varargs[untyped]): untyped =
    doAssert e.kind == nnkArgList
    if e.len > 1: # it's the fake arguments
      doAssert e.len == 4, "four ExprEqExpr nodes"
      for n in e.items:
        doAssert n.kind == nnkExprEqExpr
    bindSym"true"

  doAssert test(a)
  doAssert test(fake=90, arguments=80, also="false", possible=true)

echo "success"