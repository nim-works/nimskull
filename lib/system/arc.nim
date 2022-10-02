#
#
#            Nim's Runtime Library
#        (c) Copyright 2019 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

#[
In this new runtime we simplify the object layouts a bit: The runtime type
information is only accessed for the objects that have it and it's always
at offset 0 then. The ``ref`` object header is independent from the
runtime type and only contains a reference count.

Object subtyping is checked via the generated 'name'. This should have
comparable overhead to the old pointer chasing approach but has the benefit
that it works across DLL boundaries.

The generated name is a concatenation of the object names in the hierarchy
so that a subtype check becomes a substring check. For example::

  type
    ObjectA = object of RootObj
    ObjectB = object of ObjectA

ObjectA's ``name`` is "|ObjectA|RootObj|".
ObjectB's ``name`` is "|ObjectB|ObjectA|RootObj|".

Now to check for ``x of ObjectB`` we need to check
for ``x.typ.name.hasSubstring("|ObjectB|")``. In the actual implementation,
however, we could also use a
hash of ``package & "." & module & "." & name`` to save space.

]#

when defined(gcOrc):
  const
    rcIncrement = 0b10000 # so that lowest 4 bits are not touched
    rcMask = 0b1111
    rcShift = 4      # shift by rcShift to get the reference counter

else:
  const
    rcIncrement = 0b1000 # so that lowest 3 bits are not touched
    rcMask = 0b111
    rcShift = 3      # shift by rcShift to get the reference counter

type
  RefHeader = object
    rc: int # the object header is now a single RC field.
            # we could remove it in non-debug builds for the 'owned ref'
            # design but this seems unwise.
    when defined(gcOrc):
      rootIdx: int # thanks to this we can delete potential cycle roots
                   # in O(1) without doubly linked lists
    when defined(nimArcDebug) or defined(nimArcIds):
      refId: int

  Cell = ptr RefHeader

template head(p: pointer): Cell =
  cast[Cell](cast[int](p) -% sizeof(RefHeader))

const
  traceCollector = defined(traceArc)

when defined(nimArcDebug):
  include cellsets

  const traceId = 20 # 1037

  var gRefId: int
  var freedCells: CellSet
elif defined(nimArcIds):
  var gRefId: int

  const traceId = -1

when hasThreadSupport:
  # atomic reference counting is used when threads are enabled
  template decrement(cell: Cell; mode: AtomMemModel): untyped =
    discard atomicDec(cell.rc, rcIncrement, mode)
  template increment(cell: Cell): untyped =
    discard atomicInc(cell.rc, rcIncrement, ATOMIC_ACQ_REL)
  template count(x: Cell): untyped =
    atomicLoadN(x.rc.addr, ATOMIC_ACQUIRE) shr rcShift
else:
  # faster reference counting is used when threads are disabled
  template decrement(cell: Cell; mode: AtomMemModel): untyped =
    dec(cell.rc, rcIncrement)
  template increment(cell: Cell): untyped =
    inc(cell.rc, rcIncrement)
  template count(x: Cell): untyped =
    x.rc shr rcShift

proc nimNewObj(size, alignment: int): pointer {.compilerRtl.} =
  let hdrSize = align(sizeof(RefHeader), alignment)
  let s = size + hdrSize
  when defined(nimscript):
    discard
  else:
    result = alignedAlloc0(s, alignment) +! hdrSize
  when defined(nimArcDebug) or defined(nimArcIds):
    head(result).refId = gRefId
    discard atomicInc gRefId
    if head(result).refId == traceId:
      writeStackTrace()
      cfprintf(cstderr, "[nimNewObj] %p %ld\n", result, head(result).count)
  when traceCollector:
    cprintf("[Allocated] %p result: %p\n", result -! sizeof(RefHeader), result)

proc nimNewObjUninit(size, alignment: int): pointer {.compilerRtl.} =
  # Same as 'newNewObj' but do not initialize the memory to zero.
  # The codegen proved for us that this is not necessary.
  let hdrSize = align(sizeof(RefHeader), alignment)
  let s = size + hdrSize
  when defined(nimscript):
    discard
  else:
    result = cast[ptr RefHeader](alignedAlloc(s, alignment) +! hdrSize)
  head(result).rc = 0
  when defined(gcOrc):
    head(result).rootIdx = 0
  when defined(nimArcDebug):
    head(result).refId = gRefId
    discard atomicInc gRefId
    if head(result).refId == traceId:
      writeStackTrace()
      cfprintf(cstderr, "[nimNewObjUninit] %p %ld\n", result, head(result).count)

  when traceCollector:
    cprintf("[Allocated] %p result: %p\n", result -! sizeof(RefHeader), result)

proc nimDecWeakRef(p: pointer) {.compilerRtl, inl.} =
  decrement head(p), ATOMIC_ACQ_REL

proc nimIncRef(p: pointer) {.compilerRtl, inl.} =
  when defined(nimArcDebug):
    if head(p).refId == traceId:
      writeStackTrace()
      cfprintf(cstderr, "[IncRef] %p %ld\n", p, head(p).count)

  increment head(p)
  when traceCollector:
    cprintf("[INCREF] %p\n", head(p))

when not defined(gcOrc) or defined(nimThinout):
  proc unsureAsgnRef(dest: ptr pointer, src: pointer) {.inline.} =
    # This is only used by the old RTTI mechanism and we know
    # that 'dest[]' is nil and needs no destruction. Which is really handy
    # as we cannot destroy the object reliably if it's an object of unknown
    # compile-time type.
    dest[] = src
    if src != nil: nimIncRef src

when not defined(nimscript) and defined(nimArcDebug):
  proc deallocatedRefId*(p: pointer): int =
    ## Returns the ref's ID if the ref was already deallocated. This
    ## is a memory corruption check. Returns 0 if there is no error.
    let c = head(p)
    if freedCells.data != nil and freedCells.contains(c):
      result = c.refId
    else:
      result = 0

proc nimRawDispose(p: pointer, alignment: int) {.compilerRtl.} =
  when not defined(nimscript):
    when traceCollector:
      cprintf("[Freed] %p\n", p -! sizeof(RefHeader))
    when defined(nimOwnedEnabled):
      if head(p).rc >= rcIncrement:
        cstderr.rawWrite "[FATAL] dangling references exist\n"
        quit 1
    when defined(nimArcDebug):
      # we do NOT really free the memory here in order to reliably detect use-after-frees
      if freedCells.data == nil: init(freedCells)
      freedCells.incl head(p)
    else:
      let hdrSize = align(sizeof(RefHeader), alignment)
      alignedDealloc(p -! hdrSize, alignment)

template `=dispose`*[T](x: owned(ref T)) = nimRawDispose(cast[pointer](x), T.alignOf)
#proc dispose*(x: pointer) = nimRawDispose(x)

proc nimDestroyAndDispose(p: pointer) {.compilerRtl, raises: [].} =
  let rti = cast[ptr PNimTypeV2](p)
  if rti.destructor != nil:
    cast[DestructorProc](rti.destructor)(p)
  when false:
    cstderr.rawWrite cast[ptr PNimTypeV2](p)[].name
    cstderr.rawWrite "\n"
    if d == nil:
      cstderr.rawWrite "bah, nil\n"
    else:
      cstderr.rawWrite "has destructor!\n"
  nimRawDispose(p, rti.align)

when defined(gcOrc):
  when defined(nimThinout):
    include cyclebreaker
  else:
    include orc
    #include cyclecollector

proc nimDecRefIsLast(p: pointer): bool {.compilerRtl, inl.} =
  if p != nil:
    var cell = head(p)

    when defined(nimArcDebug):
      if cell.refId == traceId:
        writeStackTrace()
        cfprintf(cstderr, "[DecRef] %p %ld\n", p, cell.count)

    if cell.count == 0:
      result = true
      when traceCollector:
        cprintf("[ABOUT TO DESTROY] %p\n", cell)
    else:
      decrement cell, ATOMIC_RELEASE
      # According to Lins it's correct to do nothing else here.
      when traceCollector:
        cprintf("[DECREF] %p\n", cell)

proc GC_unref*[T](x: ref T) =
  ## New runtime only supports this operation for 'ref T'.
  var y {.cursor.} = x
  `=destroy`(y)

proc GC_ref*[T](x: ref T) =
  ## New runtime only supports this operation for 'ref T'.
  if x != nil: nimIncRef(cast[pointer](x))

when not defined(gcOrc):
  template GC_fullCollect* =
    ## Forces a full garbage collection pass. With `--gc:arc` a nop.
    discard

template setupForeignThreadGc* =
  ## With `--gc:arc` a nop.
  discard

template tearDownForeignThreadGc* =
  ## With `--gc:arc` a nop.
  discard

proc isObj(obj: PNimTypeV2, subclass: cstring): bool {.compilerRtl, inl.} =
  proc strstr(s, sub: cstring): cstring {.header: "<string.h>", importc.}

  result = strstr(obj.name, subclass) != nil

proc chckObj(obj: PNimTypeV2, subclass: cstring) {.compilerRtl.} =
  # checks if obj is of type subclass:
  if not isObj(obj, subclass): sysFatal(ObjectConversionDefect, "invalid object conversion")
