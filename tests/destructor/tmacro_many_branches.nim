discard """
  cmd: '''nim c --gc:arc $file'''
  description: '''
  . From https://github.com/nim-lang/Nim/issues/17198
    [ARC] Compiler crashes with a case statement with more than ~2000 branches
  . It's because of the way the DFA is built, the branches lead to nested forks,
    leading to the graph becoming wider and wider for each additional branch.
    This means that when it comes time to collectLastReads it ends up recursing
    and hitting the call limit.
  . This alters the DFA control flow graph generation for case statments.
    Gotos are now generated as a chained link, this ensures that evaluation
    of variant branches collapses as early as possible, without hitting the
    2k call limit.
  '''
"""

import std/macros

macro bigCaseStmt(arg: untyped): untyped =
  result = nnkCaseStmt.newTree(arg)

  # try to change 2000 to a bigger value if it doesn't crash
  for x in 0 ..< 2000:
    result.add nnkOfBranch.newTree(newStrLitNode($x), newStrLitNode($x))

  result.add nnkElse.newTree(newStrLitNode("other"))

macro bigIfElseExpr(): untyped =
  result = nnkIfExpr.newTree()

  for x in 0 ..< 1000:
    result.add nnkElifExpr.newTree(newLit(false), newStrLitNode($x))

  result.add nnkElseExpr.newTree(newStrLitNode("other"))

proc test(arg: string): string =
  doAssert bigIfElseExpr() == "other"

  result = bigCaseStmt(arg)

discard test("test")