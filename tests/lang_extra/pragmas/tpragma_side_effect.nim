discard """
  errormsg: "'testEpo' can have side effects"
  line: 14
  description: '''
    . From https://github.com/nim-lang/Nim/issues/13306
      I can use times.epochTime() in func
    . I write a pure func that call times.epochTime() and Nim compiles it
      without error or warning.
  '''
"""

import times

func testEpo(x: float): float = epochTime() + x


