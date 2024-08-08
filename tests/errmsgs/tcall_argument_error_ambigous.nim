discard """
  description: '''
    Ensure no "ambiguous call" error is reported when an argument, of which
    the type is needed for disambiguating, is erroneous
  '''
  cmd: "$nim check --hints:off $options $file"
  action: reject
  nimoutfull: true
  nimout: '''
tcall_argument_error_ambigous.nim(20, 14) Error: undeclared identifier: 'missing'
'''
"""

proc p(x: int, y: float): int =
  discard

proc p(x: int, y: int): int =
  discard

discard p(1, missing)
