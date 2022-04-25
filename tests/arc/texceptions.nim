discard """
  target: "cpp"
  matrix: "--gc:arc"
  knownIssue: '''this should work in CPP, see PR:
https://github.com/nim-works/nimskull/pull/290
'''
"""

block: # issue #13071
  type MyExcept = object of CatchableError
  proc gun()=
    raise newException(MyExcept, "foo:")
  proc fun()=
    var a = ""
    try:
      gun()
    except Exception as e:
      a = e.msg & $e.name # was segfaulting here for `nim cpp --gc:arc`
    doAssert a == "foo:MyExcept"
  fun()
