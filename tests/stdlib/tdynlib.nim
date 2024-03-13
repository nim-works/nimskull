discard """
  targets: c
  action: compile
"""

import std/dynlib

# ensure that the dynlib procedures are GC safe and don't have effects
proc dynlibProcsHaveNoEffects1() {.gcsafe, raises: [], tags: [].} =
  discard loadLib("")
  discard loadLib()
  unloadLib(nil)
  discard symAddr(nil, nil)

  var candidates: seq[string]
  libCandidates("", candidates)

  discard loadLibPattern("")

proc dynlibProcsHaveNoEffects2() {.gcsafe, tags: [].} =
  discard checkedSymAddr(nil, nil)
