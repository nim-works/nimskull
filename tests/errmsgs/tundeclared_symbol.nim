discard """
  cmd: '''nim check --hints:off $file'''
  action: reject
  nimout: '''
tundeclared_symbol.nim(12, 17) Error: symbol used before declaration: 'c' [generated in tundeclared_symbol.nim(12, 17)]
'''
"""

import std/macros

macro useBeforeDecl(): untyped =
  let c = genSym("c")
  result = c

useBeforeDecl()
