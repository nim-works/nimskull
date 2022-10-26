discard """
cmd: "nim doc --hints:off $file"
action: "compile"
joinable: false
description: '''
  . From https://github.com/nim-lang/Nim/issues/15916
    docgen fatal: result[0].kind == nkSym [AssertionDefect]
    '''
"""

type
  Test* = object
    id: int

proc initTest*(id: int): Test =
  result.id = id

proc hello*() =
  runnableExamples:
    discard

