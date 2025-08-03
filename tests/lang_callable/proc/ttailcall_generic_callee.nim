discard """
  description: '''
    Ensures that calls where the callee is a .tailcall procval whose type
    comes from a generic instantiation work.
  '''
  matrix: "--hints:off --showir:mir_in:test_expression --showir:mir_in:test_statement"
  targets: "c"
  nimoutFull: true
  nimout: '''
-- MIR: test_expression
scope:
  def callee: Proc[system.int] = other1
  tail callee(arg 1)
  return

-- end
-- MIR: test_statement
scope:
  def callee: Proc[system.void] = other2
  tail callee()
  return

-- end
'''
"""

type Proc[T] = proc(x: T): T {.tailcall.}

proc other1(x: int): int {.tailcall.} = x
proc other2() {.tailcall.} = discard

proc test_expression(): int {.tailcall.} =
  let callee: Proc[int] = other1
  callee(1)

proc test_statement() {.tailcall.} =
  let callee: Proc[void] = other2
  callee()

doAssert test_expression() == 1
test_statement()
