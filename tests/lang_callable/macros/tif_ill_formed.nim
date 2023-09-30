discard """
  description: '''
    Ensure that ill-formed if-statement AST created by macros is detected
  '''
  cmd: "nim check --hints:off $options $file"
  nimoutfull: "true"
  action: reject
"""
import std/macros

macro t1(): untyped =
  result =
    nnkIfStmt.newTree(
      newNimNode(nnkElseExpr)) #[tt.Error
               ^ (SemIllformedAst)]#

t1()
