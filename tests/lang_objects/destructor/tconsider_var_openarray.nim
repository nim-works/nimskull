discard """
  description: '''
    Ensure that an openArray created via ``toOpenArray`` and passed to a
    ``var openArray`` parameter is considered as a mutation, and that the
    underlying array is treated as alive afterwards
  '''
  targets: "c js vm"
  knownIssue.vm: "`toOpenArray` is not yet supported"
"""

import mhelper

proc mut(x: var openArray[Resource]) =
  x[0] = initResource()

proc test() =
  var arr: array[1, Resource]
  mut(toOpenArray(arr, 0, 0))
  # the array must be considered alive and thus destroyed

test()
doAssert numDestroy == 1
