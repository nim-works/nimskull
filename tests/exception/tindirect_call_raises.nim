discard """
  description: '''
    Regression test for indirect calls of callables without explicit `.raises`
    specification being treated as not being able to raise.

    Refer to https://github.com/nim-works/nimskull/issues/1253
  '''
  matrix: "--panics:on"
  output: "caught"
"""

# important: the procedural type must have no explicit `.raises` specification
type Proc = proc () {.nimcall.}

proc test(x: Proc) =
  try:
    x()
  except CatchableError:
    # the exception raised from `x` was previously not caught
    echo "caught"

test(proc() =
  raise CatchableError.newException("")
)
