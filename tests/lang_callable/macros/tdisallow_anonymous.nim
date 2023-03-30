discard """
  description: '''
    Routines that require a name but have none provided are rejected
  '''
  cmd: "nim check --hints:off $options $file"
  action: reject
  nimout: '''
tdisallow_anonymous.nim(32, 10) Error: identifier expected, but found:
tdisallow_anonymous.nim(33, 10) Error: identifier expected, but found:
tdisallow_anonymous.nim(34, 10) Error: identifier expected, but found:
tdisallow_anonymous.nim(35, 10) Error: identifier expected, but found:
'''
"""

import std/macros

macro makeAnon(kind: static NimNodeKind) =
  let p = newProc(name = newEmptyNode(), procType = kind)
  if kind in {nnkProcDef, nnkFuncDef, nnkIteratorDef}:
    # the procedural value needs to be consumed
    result = newVarStmt(genSym(nskVar, "tmp"), p)
  else:
    result = p

# the following routine kinds can be anonymous (which turns them into lambda
# expressions)
makeAnon(nnkProcDef)
makeAnon(nnkFuncDef)
makeAnon(nnkIteratorDef)

# these can't
makeAnon(nnkConverterDef)
makeAnon(nnkMethodDef)
makeAnon(nnkTemplateDef)
makeAnon(nnkMacroDef)