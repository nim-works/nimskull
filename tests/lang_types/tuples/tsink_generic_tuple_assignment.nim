discard """
  description: '''
    Regression test for a bug where assigning a ``sink`` generic tuple type
    to a variable of non-``sink`` type led to the value being shallow copied.
  '''
  matrix: "--showir:mir_in:test --hints:off"
  nimout: '''
-- MIR: test
scope:
  def x: sink Tuple[system.int]
  def v: Tuple[system.int]
  v = sink x

-- end
'''
"""

type Tuple[T] = (T, T)

proc test(x: sink Tuple[int]) =
  var v: Tuple[int]
  v = x # this assignment was considered to involve a conversion

test default(Tuple[int])
