#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Rokas Kupstys
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

when defined(nimTypeNames):
  # TODO: make the heap dump feature compatible with the new V2 type
  #       information

  type InstancesInfo = array[400, (cstring, int, int)]
  proc sortInstances(a: var InstancesInfo; n: int) =
    # we use shellsort here; fast and simple
    var h = 1
    while true:
      h = 3 * h + 1
      if h > n: break
    while true:
      h = h div 3
      for i in countup(h, n - 1):
        var v = a[i]
        var j = i
        while a[j - h][2] < v[2]:
          a[j] = a[j - h]
          j = j - h
          if j < h: break
        a[j] = v
      if h == 1: break

  iterator dumpHeapInstances*(): tuple[name: cstring; count: int; sizes: int] =
    ## Iterate over summaries of types on heaps.
    ## This data may be inaccurate if allocations
    ## are made by the iterator body.
    if strDesc.nextType == nil:
      strDesc.nextType = nimTypeRoot
      strDesc.name = "string"
      nimTypeRoot = addr strDesc
    var it = nimTypeRoot
    while it != nil:
      if (it.instances > 0 or it.sizes != 0):
        yield (it.name, it.instances, it.sizes)
      it = it.nextType

  proc dumpNumberOfInstances* =
    var a: InstancesInfo
    var n = 0
    var totalAllocated = 0
    for it in dumpHeapInstances():
      a[n] = it
      inc n
      inc totalAllocated, it.sizes
    sortInstances(a, n)
    for i in 0 .. n-1:
      c_fprintf(cstdout, "[Heap] %s: #%ld; bytes: %ld\n", a[i][0], a[i][1], a[i][2])
    c_fprintf(cstdout, "[Heap] total number of bytes: %ld\n", totalAllocated)
    when defined(nimTypeNames):
      let (allocs, deallocs) = getMemCounters()
      c_fprintf(cstdout, "[Heap] allocs/deallocs: %ld/%ld\n", allocs, deallocs)

template decTypeSize(cell, t) =
  when defined(nimTypeNames):
    if t.kind in {tyString, tySequence}:
      let data = cast[ptr NimSeqV2Reimpl](cellToUsr(cell)).data
      let size =
        if t.kind == tyString:
          cap + 1 + GenericSeqSize
        else:
          align(GenericSeqSize, t.base.align) + cap * t.base.size
      atomicDec t.sizes, size+sizeof(Cell)
    else:
      atomicDec t.sizes, t.base.size+sizeof(Cell)
    atomicDec t.instances

template incTypeSize(typ, size) =
  when defined(nimTypeNames):
    discard atomicInc typ.instances
    atomicInc typ.sizes, size+sizeof(Cell)