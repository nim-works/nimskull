discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/7534
    case expression with if generates AST nkIfStmt without typ, breaking JS backend
  . given the following snippet, the c backend prints correctly while
    the js backend prints undefined
  . The underlying cause can be seen in the AST.
    For `case`, a nkCaseStmt is generated, and within it, a `nkIfStmt`
    that doesn't have its typ `set`, even though the right thing
    to do in this case would be to generate an nkIfExpr with typ set
    (it's an expression after all.. or just deprecate nkIfExpr,
    but at least set the typ)"
    '''
"""
proc f(x: int): int =
  result = case x
    of 1: 2
    elif x == 2: 3
    else: 1

doAssert 2 == f(f(f(f(1))))

