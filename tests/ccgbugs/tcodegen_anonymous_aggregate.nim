discard """
targets: "c cpp"
labels: "atomics pragma union"
description: '''
  . From https://github.com/nim-lang/Nim/issues/13062
    C++ Atomics in union: ::<unnamed union>::<unnamed struct>::field
    with constructor not allowed in anonymous aggregate
  . The folloing compiles with C backend, but not with C++
  . Works in Nim 1.2.6 and above
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
doAssert x.repr == "[p = nil]"