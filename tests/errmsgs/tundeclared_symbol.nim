discard """
  cmd: '''nim check --msgFormat:sexp --filenames:canonical --hints:off $file'''
  action: reject
  nimoutFormat: sexp
"""

import std/macros

macro useBeforeDecl(): untyped =
  let c = genSym("c")#[tt.Error
               ^ (SemUndeclaredSymUsed)]#
  result = c

useBeforeDecl()
