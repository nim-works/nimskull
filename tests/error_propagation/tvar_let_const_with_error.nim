discard """
  cmd: "nim check --hints:off $options $file"
  action: reject
  nimoutFull: true
  nimout: '''
tvar_let_const_with_error.nim(12, 12) Error: undeclared identifier: 'missing'
tvar_let_const_with_error.nim(13, 12) Error: undeclared identifier: 'missing'
tvar_let_const_with_error.nim(14, 12) Error: undeclared identifier: 'missing'
'''
"""

var va   = missing
let le   = missing
const co = missing

# using the symbols doesn't result in errors being reported to the programmer
echo va, le, co
