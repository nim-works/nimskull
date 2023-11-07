import dom

discard Node()

block discard_pointer_returning_call:
  # discarding a call returning a pointer-like value previously
  # resulted in incorrect code being generated
  var i = 0

  proc get(x: pointer): pointer =
    inc i
    x

  discard get(nil)
  # make sure that the call was really executed
  doAssert i == 1, "`get` was not evaluated?"