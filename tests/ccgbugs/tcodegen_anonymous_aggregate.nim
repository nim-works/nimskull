discard """
targets: "c"
labels: "atomics pragma union"
description: '''
  . From https://github.com/nim-lang/Nim/issues/13062
    originally a C++, keep to test for C regressions.
'''
"""

import atomics

type
  Pledge* {.exportc.} = object
    p: PledgePtr

  PledgeKind {.exportc.} = enum
    Single
    Iteration

  PledgePtr {.exportc.} = ptr object
    case kind: PledgeKind
    of Single:
      impl: PledgeImpl
    of Iteration:
      discard

  PledgeImpl {.exportc.} = object
    fulfilled: Atomic[bool]

var x: Pledge
doAssert x.p == nil