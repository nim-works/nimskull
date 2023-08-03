discard """
  description: "From https://github.com/nim-lang/Nim/issues/15667: Confusing 'invalid indentation' errors reported when indentation is not at fault"
  cmd: "nim check $options $file"
  action: "reject"
  nimout: '''
tparse_confusing_identation.nim(23, 5) Error: invalid indentation, maybe missing '=' at tparse_confusing_identation.nim(22, 13)?
tparse_confusing_identation.nim(28, 5) Error: invalid indentation, maybe missing '=' at tparse_confusing_identation.nim(26, 13)?
tparse_confusing_identation.nim(33, 5) Error: invalid indentation, maybe missing '=' at tparse_confusing_identation.nim(31, 25)?
tparse_confusing_identation.nim(42, 5) Error: invalid indentation, maybe missing '=' at tparse_confusing_identation.nim(38, 12)?
tparse_confusing_identation.nim(56, 5) Error: invalid indentation, maybe missing '=' at tparse_confusing_identation.nim(55, 13)?
tparse_confusing_identation.nim(61, 48) Error: expression expected, but found ','
'''
"""






# line 20
block:
  proc fn1()
    discard

block:
  proc fn2()
    #
    discard

block:
  proc fn3() {.exportc.}
    #
    discard

block: # complex example
  proc asdfasdfsd() {. exportc,
      inline
         .}     # foo
    #[
    bar
    ]#
    discard

block: # xxx this doesn't work yet (only a bare `invalid indentation` error)
  proc fn5()
    ##
    discard

block:
  proc fn6*()
    ## foo bar
    runnableExamples: discard

block:
  proc fn8()
    runnableExamples:
      discard
    discard

# semiStmtList loop issue
proc bar(k:static bool):SomeNumber = (when k: 3, else: 3.0)

