discard """
  output: ""
  joinable: false
  description: '''
  . if `Thread` is freed before the thread finishes, SIGSEGV
  . reference counting of `Thread.coreFn` is broken
'''
"""

import std/os

var ok = false
proc whatever() =
  ok = true

var thread: Thread[void]
createThread(thread, whatever)

# Simulated: variable going out of scope
thread.reset()

# this breaks as well
# thread.dataFn.reset()

while not ok:
  discard

