discard """
  targets: "c js"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/6163
      Wrong JavaScript code generation
    . nim c works , but nim js does not.
    . https://github.com/nim-lang/Nim/pull/8936
      JS strings have no trailing zero
  '''
"""

from std/sugar import `->`, `=>`
from std/math import `^`, sum
from std/sequtils import filter, map, toSeq

proc f: int =
  toSeq(10..<10_000).filter(a => a == ($a).map(d => (d.ord-'0'.ord).int^4).sum).sum

var a = f()

doAssert a == 19316
