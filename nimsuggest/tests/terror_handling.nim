# Test ill formed AST will not causes crash and subsequent errors

import std/macros

macro t1(): untyped =
  result = newNimNode(nnkForStmt)
  result.add(ident("i")) 
  result.add newNimNode(nnkCall)
  result.add nnkDiscardStmt.newTree(newEmptyNode())

t1()

#[!]#
discard """
$nimsuggest --tester $file
>chk $1
chk;;skUnknown;;;;Error;;$file;;8;;23;;"illformed AST: ()";;0
"""
