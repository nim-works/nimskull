#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# NimSkull high-level memory manager. Currently, it only supports the native
# NimSkull GC.

{.push checks:off.}

const
  leakDetector = defined(nimLeakDetector)
  overwriteFree = defined(nimBurnFree) # overwrite memory with 0xFF before free
  trackAllocationSource = leakDetector

  reallyOsDealloc = true
  coalescRight = true
  coalescLeft = true
  logAlloc = false
  useCellIds = defined(nimCorruption)

type
  PPointer = ptr pointer

when declared(IntsPerTrunk):
  discard
else:
  include bitmasks

proc raiseOutOfMem() {.noinline.} =
  if outOfMemHook != nil: outOfMemHook()
  cstderr.rawWrite("out of memory\n")
  quit(1)

when defined(useMalloc):
  include system / mm / malloc

else:
  include system / alloc

  var allocator {.rtlThreadVar.}: MemRegion
  instantiateForRegion(allocator)

when not declared(ForeignCell):
  type ForeignCell* = object
    data*: pointer

  proc protect*(x: pointer): ForeignCell = ForeignCell(data: x)
  proc dispose*(x: ForeignCell) = discard
  proc isNotForeign*(x: ForeignCell): bool = false

{.pop.}