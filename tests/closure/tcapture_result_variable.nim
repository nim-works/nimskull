discard """
  errormsg: "The variable name cannot be `result`!"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/15594
      cannot capture result, produces unhelpful error
    . Since this shouldn't compile, I will document or
      error that capture can't capture result variable.
  '''
"""

import sugar

proc begin(): int =
  capture result:
    echo 1+1
  result