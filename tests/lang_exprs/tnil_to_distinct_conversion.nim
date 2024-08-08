discard """
  description: '''
    Ensure that converting 'nil' literals to ``distinct pointer`` works
  '''
  matrix: "--hints:off --showir:mir_in:test"
  nimoutFull: true
  nimout: '''
-- MIR: test
scope:
  def a: Ptr = nil
  def b: Ptr = nil

-- end
'''
"""

type Ptr = distinct pointer

proc test() =
  var a = Ptr(nil) # direct conversions
  var b = Ptr(pointer(nil)) # with intermediate conversion

test()
