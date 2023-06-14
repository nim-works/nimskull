#
#
#            Nim's Runtime Library
#        (c) Copyright 2019 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# Cell seqs for cyclebreaker and orc.

type
  CellTuple[T] = (T, PNimTypeV2)
  CellArray[T] = ptr UncheckedArray[CellTuple[T]]
  CellSeq[T] = object
    len, cap: int
    d: CellArray[T]

proc add[T](s: var CellSeq[T], c: T; t: PNimTypeV2) {.inline.} =
  if s.len >= s.cap:
    s.cap = s.cap * 3 div 2
    when compileOption("threads"):
      var d = cast[CellArray[T]](allocShared(uint(s.cap * sizeof(CellTuple[T]))))
    else:
      var d = cast[CellArray[T]](alloc(s.cap * sizeof(CellTuple[T])))
    copyMem(d, s.d, s.len * sizeof(CellTuple[T]))
    when compileOption("threads"):
      deallocShared(s.d)
    else:
      dealloc(s.d)
    s.d = d
    # XXX: realloc?
  s.d[s.len] = (c, t)
  inc(s.len)

proc init[T](s: var CellSeq[T], cap: int = 1024) =
  s.len = 0
  s.cap = cap
  when compileOption("threads"):
    s.d = cast[CellArray[T]](allocShared(uint(s.cap * sizeof(CellTuple[T]))))
  else:
    s.d = cast[CellArray[T]](alloc(s.cap * sizeof(CellTuple[T])))

proc deinit[T](s: var CellSeq[T]) =
  if s.d != nil:
    when compileOption("threads"):
      deallocShared(s.d)
    else:
      dealloc(s.d)
    s.d = nil
  s.len = 0
  s.cap = 0

proc pop[T](s: var CellSeq[T]): (T, PNimTypeV2) =
  result = s.d[s.len-1]
  dec s.len
