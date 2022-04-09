discard """
  targets: native
  errormsg: "expected a template that takes 3 arguments"
  line: 17
"""

import macros

template grainBlock(proxyTypeName: untyped, proxyProcs: untyped): typed =
  discard

var
  proxyTypeName: string
  proxyProcs: string

macro foo(): untyped =
  let x = getAst grainBlock(proxyTypeName, proxyProcs, proxyTypeName)

foo()

