discard """
  output: '''
In doStuff()
In initProcess()
initProcess() done
TEST
Crashes before getting here!
'''
  joinable: false
  description: '''
  . From https://github.com/nim-lang/Nim/issues/7172
    (Unintended) Destruction of Thread object causes hard to debug crash
    Thread object being destroyed, while the thread is still running.
    In this particular case, I believe the Thread object was on the stack,
    but I assume the same would happen with a GCed Thread object.
  . If "var thread" is moved to the global scope, the crash does not happen.
'''
"""

import std/os

proc whatever() {.thread, nimcall.} =
  echo("TEST")

proc initProcess(): void =
  echo("In initProcess()")
  var thread: Thread[void]
  createThread(thread, whatever)
  echo("initProcess() done")
  joinThread(thread)

proc doStuff(): void =
  echo("In doStuff()")
  # ...
  initProcess()
  sleep(500)
  # ...
  echo("Crashes before getting here!")

doStuff()

