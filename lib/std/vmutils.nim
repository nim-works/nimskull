##[
Experimental API, subject to change.
]##

when defined(vm):
  {.pragma: vmOnly.}
else:
  {.pragma: vmOnly, compileTime.}

proc vmTrace*(on: bool) {.vmOnly.} =
  runnableExamples:
    static: vmTrace(true)
    proc fn =
      var a = 1
      vmTrace(false)
    static: fn()
