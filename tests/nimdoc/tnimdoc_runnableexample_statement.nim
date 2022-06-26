discard """
  cmd: "nim doc -r $file"
  errormsg: "runnableExamples must appear before the first non-comment statement"
  line: 14
  description: '''
    . From https://github.com/nim-lang/Nim/issues/17615
      runnableExamples silently ignored if placed after some code.
    '''
"""

func fn*() =
  ## foo
  discard
  runnableExamples:
    assert true

