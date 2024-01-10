discard """
  joinable: false
  knownIssue.vm: "`setCurrentException` is not supported"
"""

# refs https://github.com/nim-lang/Nim/pull/18247#issuecomment-860877161

let ex = newException(CatchableError, "test")
setCurrentException(ex)
doAssert getCurrentException().msg == ex.msg
doAssert getCurrentExceptionMsg() == ex.msg
