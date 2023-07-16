discard """
  targets: "c !js !vm"
  description: '''
    Run-time expressions used with the `.dynlib` pragma are also affected by
    destructor injection
  '''
  outputsub: "could not load: non_existent_library_name"
  exitcode: 1
"""

import mhelper

proc loadProc(r: Resource, name: cstring): pointer =
  doAssert r.content
  doAssert name == "imported1"
  result = nil # the value doesn't matter

# --- test custom importer calls:
proc imported1() {.importc, dynlib: loadProc((var r = initResource(); r), "").}
# `r` is destroyed at the end of the loader logic for `imported1`

# use the procedure for the loader logic to be generated
imported1()

# --- test run-time computed library name:
template getName(): string =
  block:
    doAssert numDestroy == 1, "`r` wasn't destroyed"
    block:
      var r2 = initResource()
    doAssert numDestroy == 2, "`r2` wasn't destroyed"

  "non_existent_library_name" # will cause an error

proc imported2() {.importc, dynlib: getName().}

imported2()