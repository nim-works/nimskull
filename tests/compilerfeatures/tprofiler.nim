discard """
  description: '''
    Ensure that the built-in instrumentation with profiler callback calls
    works
  '''
  targets: "c js vm"
  matrix: "--profiler:on"
  knownIssue.js vm: '''
    The `system/profile.nim` module is not available for the targets
  '''
"""

var
  traces: array[3, StackTrace]
  enabled = true
  numTraces = 0

# instrumentation needs to be disabled for the callbacks, otherwise there'd be
# an infinite recursion
{.push profiler: off.}

proc enabledCallback(): bool =
  result = enabled
  # XXX: an issue with the profiler runtime requires disabling the callback
  #      until the `profileCallback` is done
  enabled = false

proc profileCallback(st: StackTrace) =
  traces[numTraces] = st
  inc numTraces
  enabled = true # re-enable

{.pop.}

# nothing will happen before the hook is set
profilerHook = profileCallback
# the "profiling requested" callback guards whether to invoke the profiler
# callback
profilingRequestedHook = enabledCallback

proc test() =
  # the callback is invoked when a procedure is entered
  var i = 0
  while i < 2:
    inc i
    # the callback is also invoked at the end of while loop's body

test() # run once

proc testPure() {.asmNoStackFrame.} =
  # pure routines aren't instrumented
  var i = 0
  while i < 2:
    inc i

# disable the callback so that the traces can be inspected
enabled = false

# validate the traces:
doAssert numTraces == 3
# the end of the list is signaled by a nil cstring
doAssert traces[0].lines[0..2] == [cstring"test", "tprofiler", nil]
doAssert traces[0].files[0..2] == [cstring"tprofiler.nim", "tprofiler.nim", nil]