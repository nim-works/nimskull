discard """
  description: '''
  . From https://github.com/nim-lang/Nim/issues/8550
    Iterating closure iterator in nested function is empty
  . This is not the same but similar to #7787
  . The problem is in transformFor, when c.tooEarly is set
    the transform drops the body and replaces it with a nkEmpty node
    and that's why no code is emitted at all.
  . The tooEarly variable is set in liftLambdas when the procedure body
    is empty or when the function owner is not a skModule, like it happens
    in this case. Sadly I don't get what that flag is for.
'''
"""

proc chk_fail(): seq[string] =
  iterator x(): int {.closure.} = yield 42
  proc f(cl: iterator(): int {.closure.}): seq[string] =
    result = @[]
    for i in cl(): result.add($i)
  result = f(x)

let representation = $( chk_fail() )
doAssert representation == "@[\"42\"]"