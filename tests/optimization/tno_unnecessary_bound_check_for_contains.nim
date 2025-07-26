discard """
  action: compile
  matrix: "--showir:mir_in:test --checks:off"
  nimout: '''-- MIR: test
scope:
  def _2: bool
  def _3: bool = leI(arg 0'i8, arg x)
  if _3:
    _2 := inSet(arg <D0>, arg x)
  result = _2
  goto [L1]
L1:
return result

-- end'''
"""

proc test(x: int8): bool {.exportc.} =
  x in {1, 2, 3}
