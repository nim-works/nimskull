discard """
  cmd: "nim check --hints:off $options $file"
  action: reject
  nimoutFull: true
  nimout: '''
tproc_with_error.nim(11, 11) Error: undeclared identifier: 'missing'
'''
"""

proc p() =
  discard missing # <- error

# calling the procedure doesn't result in errors being reported to the
# programmer
p()
