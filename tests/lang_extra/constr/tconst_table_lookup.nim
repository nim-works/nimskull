discard """
  action: "run"
  description: '''
  . From https://github.com/nim-lang/Nim/issues/7936
    toSeq(lookupTable.values) seems to be causing a SIGSEGV
  . Constant folding is confused by method calls: A nkDotExpr may
    have a nkIdent as second son in such a case.
  . Use the resolved typedesc in semVarOrLet: By leaving the
    unsemanticized node in the AST we'd trip some passes like the
    Transf one seen here.
'''
"""


import
  tables, deques, sequtils

const
  lookupTable = {'(': ')', '{': '}', '[': ']'}.toTable

proc isPaired*(value: string): bool =
  var stack = initDeque[char]()

  for item in value:
    # echo "Looking at " & item
    if item in lookupTable:
      stack.addLast(item)
    if item in toSeq(lookupTable.values):
      if stack.len == 0:
        return false
      if lookupTable[stack.popLast()] != item:
        return false

  return stack.len == 0

doAssert isPaired("{[()]}") == true
doAssert isPaired("a)b(c") == false