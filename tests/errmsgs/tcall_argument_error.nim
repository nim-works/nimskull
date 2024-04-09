discard """
  description: '''
    Ensure no redundant errors are reported in check mode when a call argument
    has an error
  '''
  cmd: "$nim check --hints:off $options $file"
  action: reject
  nimoutfull: true
  nimout: '''
tcall_argument_error.nim(44, 14) Error: undeclared identifier: 'missing'
tcall_argument_error.nim(46, 21) Error: undeclared identifier: 'missing'
tcall_argument_error.nim(48, 11) Error: undeclared identifier: 'missing'
tcall_argument_error.nim(48, 10) Error: type mismatch: got <>
but expected one of:
proc p(x, y: int): int
  first type mismatch at position: 2
  missing parameter: y
proc p(x, y: string): string
  first type mismatch at position: 2
  missing parameter: y

expression: p(missing)
tcall_argument_error.nim(49, 11) Error: undeclared identifier: 'missing'
tcall_argument_error.nim(49, 10) Error: type mismatch: got <>
but expected one of:
proc p(x, y: int): int
  first type mismatch at position: 3
  extra argument given
proc p(x, y: string): string
  first type mismatch at position: 2
  required type for y: string
  but expression '1' is of type: int literal(1)

expression: p(missing, 1, 2)
'''
"""

proc p(x, y: string): string =
  discard

proc p(x, y: int): int =
  discard

discard p(1, missing)
# nested call, where the inner call expression has an error:
discard p("", p("", missing))
# if an argument is erroneous, arity is still considered:
discard p(missing) # no overload with matching arity
discard p(missing, 1, 2) # no overload with matching arity
