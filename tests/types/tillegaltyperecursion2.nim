discard """
  targets: native
  errormsg: "illegal recursion in type 'Executor'"
  line: 9
"""
# bug reported by PR #5637
type
  Executor[N] = Executor[N]
var e: Executor[int]
