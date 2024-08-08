discard """
  description: "Ensure that thread parameters are destroyed properly"
  joinable: false
"""

var counter: int

type Param = object
  init: bool

proc `=destroy`(x: var Param) =
  if x.init:
    discard atomicInc(counter, 1, ATOMIC_SEQ_CST)

proc run(x: Param) {.thread.} =
  discard

var thread: Thread[Param]
thread.createThread(run, Param(init: true))
thread.joinThread()

doAssert atomicLoadN(addr counter, ATOMIC_SEQ_CST) == 1
