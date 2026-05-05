discard """
  output: '''
-100
-100,000
100,000
'''
"""
# test https://github.com/nim-lang/Nim/issues/11352

import std/strutils
echo insertSep($(-100), ',')
echo insertSep($(-100_000), ',')
echo insertSep($(100_000), ',')
