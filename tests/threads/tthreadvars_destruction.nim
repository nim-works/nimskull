discard """
  description: '''
    Ensure that threadvars are destroyed and that the order is correct:
    1. inter-module: the module closed last has threadvars destroyed first
    2. intra-module:
      i. top-level threadvars are destroyed, in reverse order of definition
      ii. threadvars defined within routines are destroyed, in an
         unspecified order

    For the main thread, module-level threadvars are destroyed after top-level
    thread-globals but before procedure-scoped threadvars and thread-globals.
  '''
  output: "1\n2\n3\n4\n5\n6\n7\n8\n"
"""

import mthreadvars_destruction

# the threadvars and globals from this module are destoyed first
let global = Object(val: 5)
var tv1 {.threadvar.}: Object
tv1 = Object(val: 6)

proc run() {.thread.} =
  tv1 = Object(val: 1)
  var tv2 {.threadvar.}: Object
  tv2 = Object(val: 2)

  init()

var t = (createThread[void])(run)
t.joinThread()
