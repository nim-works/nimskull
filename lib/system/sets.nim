#
#
#            Nim's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Implements the set operations for large bitsets. The compiler could
## emit the code fragments manually, but that's harder to get right and the
## resulting implementation would also be not as easy to understand.

type
  SetRange = range[0'u16..7'u16]
  NimSet = ptr UncheckedArray[set[SetRange]]
  # large sets are arrays of 8-bit sets

{.push stacktrace: off, checks: off.}

proc nimSetIncl(a: NimSet, val: uint16) {.compilerproc, inline.} =
  a[val shr 3].incl(val and 0x7)

proc nimSetExcl(a: NimSet, val: uint16) {.compilerproc, inline.} =
  a[val shr 3].excl(val and 0x7)

proc nimSetIn(a: NimSet, val: uint16): bool {.compilerproc, inline.} =
  # note: the known-to-be-safe cast to the range gets rid of an
  # unnecessary branch
  cast[SetRange](val and 0x7) in a[val shr 3]

proc nimSetMinus(dest, a, b: NimSet, len: int) {.compilerproc, inline.} =
  # set difference
  for i in 0..<len:
    dest[i] = a[i] - b[i]

proc nimSetPlus(dest, a, b: NimSet, len: int) {.compilerproc, inline.} =
  # set union
  for i in 0..<len:
    dest[i] = a[i] + b[i]

proc nimSetMul(dest, a, b: NimSet, len: int) {.compilerproc, inline.} =
  # set intersection
  for i in 0..<len:
    dest[i] = a[i] * b[i]

proc nimSetLt(a, b: NimSet, len: int): bool {.compilerproc, inline.} =
  # true subset test
  for i in 0..<len:
    if not(a[i] <= b[i]):
      return false

  result = nimCmpMem(a, b, len) != 0

proc nimSetLe(a, b: NimSet, len: int): bool {.compilerproc, inline.} =
  # subset test
  for i in 0..<len:
    if not(a[i] <= b[i]):
      return false

  result = true

proc cardSet(s: NimSet, len: int): int {.compilerproc, inline.} =
  let p = cast[ptr UncheckedArray[uint8]](s)
  var i = 0
  result = 0
  when defined(x86) or defined(amd64):
    while i < len - 8:
      inc(result, countBits64((cast[ptr uint64](addr p[i]))[]))
      inc(i, 8)

  while i < len:
    inc(result, countBits32(uint32(p[i])))
    inc(i, 1)

{.pop.}
