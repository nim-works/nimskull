discard """
  description: "Only macros and templates can be used for term-rewriting"
  cmd: "nim check --hints:off $options $file"
  action: reject
  nimout: '''
tdisallow_non_macros.nim(14, 8) Error: patterns can only be specified on macros and templates
tdisallow_non_macros.nim(16, 8) Error: patterns can only be specified on macros and templates
tdisallow_non_macros.nim(18, 15) Error: patterns can only be specified on macros and templates
tdisallow_non_macros.nim(20, 16) Error: patterns can only be specified on macros and templates
tdisallow_non_macros.nim(22, 10) Error: patterns can only be specified on macros and templates
'''
"""

proc p{f(1, 1)}() = discard

func f{f(1, 1)}() = discard

iterator iter{f(1, 1)}() = discard

converter conv{f(1, 1)}() = discard

method m{f(1, 1)}() = discard