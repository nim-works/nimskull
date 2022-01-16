discard """
  action: reject
  cmd: '''nim compile --hints:off $options $file'''
  nimout: '''
tproc_mismatch_missing_param.nim(14, 10) Error: type mismatch: got <>
but expected one of:
proc withParam(arg: int)

expression: withParam()
'''
"""

proc withParam(arg: int) = discard
withParam()

