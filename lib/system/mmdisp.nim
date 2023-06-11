#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# Nim high-level memory manager: It supports Boehm's GC, Go's GC, no GC and the
# native Nim GC. The native Nim GC is the default.

#{.push checks:on, assertions:on.}
{.push checks:off.}

const
  logGC = false
  traceGC = false # extensive debugging
  alwaysCycleGC = defined(nimSmokeCycles)
  alwaysGC = defined(nimFulldebug) # collect after every memory
                                # allocation (for debugging)
  leakDetector = defined(nimLeakDetector)
  overwriteFree = defined(nimBurnFree) # overwrite memory with 0xFF before free
  trackAllocationSource = leakDetector

  cycleGC = true # (de)activate the cycle GC
  reallyDealloc = true # for debugging purposes this can be set to false
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
  include "system/alloc"

  var allocator {.rtlThreadVar.}: MemRegion
  instantiateForRegion(allocator)

when not declared(ForeignCell):
  type ForeignCell* = object
    data*: pointer

  proc protect*(x: pointer): ForeignCell = ForeignCell(data: x)
  proc dispose*(x: ForeignCell) = discard
  proc isNotForeign*(x: ForeignCell): bool = false
