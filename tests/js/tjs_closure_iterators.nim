discard """
  errormsg: "Closure iterators are not supported by JS backend!"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/7109
      Better error message when attempting to use closure iterators with js backend
    . The current error looks like there is a bug.
      It should probably just say "Closure iterators are not supported
      by the js backend
    . Current Output:
      jsiter.nim(5, 9) Error: internal error: symbol has no generated name: iter
  '''
"""

iterator iter*(): int {.closure.} =
  yield 3

var x = iter

