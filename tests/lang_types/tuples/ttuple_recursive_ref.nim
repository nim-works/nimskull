discard """
  errormsg: "illegal recursion in type 'Node'"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/7012
      Infinite recursion through semOverloadedCallAnalyseEffects with
      ref-recursive tuple
    . Changing tuple to object fixes it.
    . nim check seems happy with this code, but nim c exits silently with
      code 138
  '''
"""

type Node[T] = tuple
    next: ref Node[T]
var n: Node[int]