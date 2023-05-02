discard """
  matrix: "--threads:on -d:release -d:useRealtimeGC"
  joinable:false
  knownIssue: "the test depends on the GC not running at the wrong time"
"""

#[
Test the realtime GC without linking nimrtl.dll/so.

To build by hand and run the test for 35 minutes:
`nim r --threads:on -d:runtimeSecs:2100 tests/realtimeGC/tmain.nim`

EDIT: the test as it currently is can't work, and only by accident didn't
crash in the past. Without linking to the "nimrtl" dynlib, two instance of the
GC (one for the executable and one for the dynlib) are created. The GC instance
used for the dynlib is initialized with an incorrect stack-bottom value (due to
it being initialized from the dynlibs entry point), leading to some stack cells
not being detected, causing the referenced heap location to be garbage
collected.

]#

import times, os, strformat, strutils
from stdtest/specialpaths import buildDir

const runtimeSecs {.intdefine.} = 5

const file = "shared.nim"
const dllname = buildDir / (DynlibFormat % "shared_D20210524T180506")

static:
  # D20210524T180826:here we compile the dependency on the fly
  let nim = getCurrentCompilerExe()
  let (output, exitCode) = gorgeEx(fmt"{nim} c -o:{dllname} --debuginfo --app:lib --threads:on -d:release -d:useRealtimeGC {file}")
  doAssert exitCode == 0, output

proc status() {.importc: "status", dynlib: dllname.}
proc count() {.importc: "count", dynlib: dllname.}
proc checkOccupiedMem() {.importc: "checkOccupiedMem", dynlib: dllname.}

proc process() =
  let startTime = getTime()
  let runTime = cast[Time](runtimeSecs)
  var accumTime: Time
  while accumTime < runTime:
    for i in 0..10:
      count()
    # echo("1. sleeping... ")
    sleep(500)
    for i in 0..10:
      status()
    # echo("2. sleeping... ")
    sleep(500)
    checkOccupiedMem()
    accumTime = cast[Time]((getTime() - startTime))
    # echo("--- Minutes left to run: ", int(int(runTime-accumTime)/60))

proc main() =
  process()

main()
