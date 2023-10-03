discard """
  description: '''
    Ensure that ill-formed for-statement AST is detected
  '''
  cmd: "nim check --hints:off $options $file"
  action: reject
  nimout: '''
tfor_ill_formed.nim(18, 16) Error: expression has no type: 
if true:
  discard
else:
  discard
tfor_ill_formed.nim(18, 16) Error: iterator within for loop context expected
tfor_ill_formed.nim(18, 1) Error: wrong number of variables
'''
"""

for i, x, y in (if true: discard else: discard):
  discard
