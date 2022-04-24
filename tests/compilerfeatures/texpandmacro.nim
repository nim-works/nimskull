discard """
  matrix: "--expandMacro:foo"
  nimout: '''texpandmacro.nim(17, 1) Hint: expanded macro:
echo ["injected echo"]
var x = 4 [ExpandMacro]
'''
  output: '''injected echo'''
"""

import macros

macro foo(x: untyped): untyped =
  result = quote do:
    echo "injected echo"
    `x`

foo:
  var x = 4
