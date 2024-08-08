#
#
#            Nim's Runtime Library
#        (c) Copyright 2020 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# Cycle collector based on
# https://researcher.watson.ibm.com/researcher/files/us-bacon/Bacon01Concurrent.pdf
# And ideas from Lins' in 2008 by the notion of "critical links", see
# "Cyclic reference counting" by Rafael Dueire Lins
# R.D. Lins / Information Processing Letters 109 (2008) 71–78
#

include cellseqs_v2

const
  colBlack = 0b000
  colGray = 0b001
  colWhite = 0b010
  maybeCycle = 0b100 # possibly part of a cycle; this has to be a "sticky" bit
  jumpStackFlag = 0b1000
  colorMask = 0b011

  logOrc = defined(nimArcIds)

type
  TraceProc = proc (p, env: pointer) {.nimcall, benign, raises: [], tags: [].}
  DisposeProc = proc (p: pointer) {.nimcall, benign.}

template color(c): untyped = c.rc and colorMask
template setColor(c, col) =
  when col == colBlack:
    c.rc = c.rc and not colorMask
  else:
    c.rc = c.rc and not colorMask or col

const
  optimizedOrc = false # not defined(nimOldOrc)
# XXX Still incorrect, see tests/arc/tdestroy_in_loopcond

proc nimIncRefCyclic(p: pointer; cyclic: bool) {.compilerRtl, inl.} =
  let h = head(p)
  inc h.rc, rcIncrement
  when optimizedOrc:
    if cyclic:
      h.rc = h.rc or maybeCycle

proc nimMarkCyclic(p: pointer) {.compilerRtl, inl.} =
  when optimizedOrc:
    if p != nil:
      let h = head(p)
      h.rc = h.rc or maybeCycle

proc unsureAsgnRef(dest: ptr pointer, src: pointer) {.inline.} =
  # This is only used by the old RTTI mechanism and we know
  # that 'dest[]' is nil and needs no destruction. Which is really handy
  # as we cannot destroy the object reliably if it's an object of unknown
  # compile-time type.
  dest[] = src
  if src != nil: nimIncRefCyclic(src, true)

const
  useJumpStack = false # for thavlak the jump stack doesn't improve the performance at all

type
  GcEnv = object
    traceStack: CellSeq[ptr pointer]
    when useJumpStack:
      jumpStack: CellSeq[ptr pointer]   # Lins' jump stack in order to speed up traversals
    toFree: CellSeq[Cell]
    freed, touched, edges, rcSum: int
    keepThreshold: bool

proc trace(s: Cell; desc: PNimTypeV2; j: var GcEnv) {.inline.} =
  if desc.traceImpl != nil:
    var p = s +! sizeof(RefHeader)
    cast[TraceProc](desc.traceImpl)(p, addr(j))

include threadids

when logOrc:
  proc writeCell(msg: cstring; s: Cell; desc: PNimTypeV2) =
    cfprintf(cstderr, "%s %s %ld root index: %ld; RC: %ld; color: %ld; thread: %ld\n",
      msg, desc.name, s.refId, s.rootIdx, s.rc shr rcShift, s.color, getThreadId())

proc free(s: Cell; desc: PNimTypeV2) {.inline.} =
  when traceCollector:
    cprintf("[From ] %p rc %ld color %ld\n", s, s.rc shr rcShift, s.color)
  let p = s +! sizeof(RefHeader)

  when logOrc: writeCell("free", s, desc)

  if desc.destructor != nil:
    cast[DestructorProc](desc.destructor)(p)

  when false:
    cstderr.rawWrite desc.name
    cstderr.rawWrite " "
    if desc.destructor == nil:
      cstderr.rawWrite "lacks dispose"
      if desc.traceImpl != nil:
        cstderr.rawWrite ", but has trace\n"
      else:
        cstderr.rawWrite ", and lacks trace\n"
    else:
      cstderr.rawWrite "has dispose!\n"

  nimRawDispose(p, desc.align)

template orcAssert(cond, msg) =
  when logOrc:
    if not cond:
      cfprintf(cstderr, "[Bug!] %s\n", msg)
      quit 1

proc nimTraceRef(q: pointer; desc: PNimTypeV2; env: pointer) {.compilerRtl, inline.} =
  let p = cast[ptr pointer](q)
  if p[] != nil:
    var j = cast[ptr GcEnv](env)
    j.traceStack.add(p, desc)

proc nimTraceRefDyn(q: pointer; env: pointer) {.compilerRtl, inline.} =
  let p = cast[ptr pointer](q)
  if p[] != nil:
    var j = cast[ptr GcEnv](env)
    j.traceStack.add(p, cast[ptr PNimTypeV2](p[])[])

var
  roots {.threadvar.}: CellSeq[Cell]

proc unregisterCycle(s: Cell) =
  # swap with the last element. O(1)
  let idx = s.rootIdx-1
  when false:
    if idx >= roots.len or idx < 0:
      cprintf("[Bug!] %ld\n", idx)
      quit 1
  roots.d[idx] = roots.d[roots.len-1]
  roots.d[idx][0].rootIdx = idx+1
  dec roots.len
  s.rootIdx = 0

proc scanBlack(s: Cell; desc: PNimTypeV2; j: var GcEnv) =
  #[
  proc scanBlack(s: Cell) =
    setColor(s, colBlack)
    for t in sons(s):
      t.rc = t.rc + rcIncrement
      if t.color != colBlack:
        scanBlack(t)
  ]#
  s.setColor colBlack
  let until = j.traceStack.len
  trace(s, desc, j)
  when logOrc: writeCell("root still alive", s, desc)
  while j.traceStack.len > until:
    let (entry, desc) = j.traceStack.pop()
    let t = head entry[]
    inc t.rc, rcIncrement
    if t.color != colBlack:
      t.setColor colBlack
      trace(t, desc, j)
      when logOrc: writeCell("child still alive", t, desc)

proc markGray(s: Cell; desc: PNimTypeV2; j: var GcEnv) =
  #[
  proc markGray(s: Cell) =
    if s.color != colGray:
      setColor(s, colGray)
      for t in sons(s):
        t.rc = t.rc - rcIncrement
        if t.color != colGray:
          markGray(t)
  ]#
  if s.color != colGray:
    s.setColor colGray
    inc j.touched
    # keep in mind that refcounts are zero based so add 1 here:
    inc j.rcSum, (s.rc shr rcShift) + 1
    orcAssert(j.traceStack.len == 0, "markGray: trace stack not empty")
    trace(s, desc, j)
    while j.traceStack.len > 0:
      let (entry, desc) = j.traceStack.pop()
      let t = head entry[]
      dec t.rc, rcIncrement
      inc j.edges
      when useJumpStack:
        if (t.rc shr rcShift) >= 0 and (t.rc and jumpStackFlag) == 0:
          t.rc = t.rc or jumpStackFlag
          when traceCollector:
            cprintf("[Now in jumpstack] %p %ld color %ld in jumpstack %ld\n", t, t.rc shr rcShift, t.color, t.rc and jumpStackFlag)
          j.jumpStack.add(entry, desc)
      if t.color != colGray:
        t.setColor colGray
        inc j.touched
        # we already decremented its refcount so account for that:
        inc j.rcSum, (t.rc shr rcShift) + 2
        trace(t, desc, j)

proc scan(s: Cell; desc: PNimTypeV2; j: var GcEnv) =
  #[
  proc scan(s: Cell) =
    if s.color == colGray:
      if s.rc > 0:
        scanBlack(s)
      else:
        s.setColor(colWhite)
        for t in sons(s): scan(t)
  ]#
  if s.color == colGray:
    if (s.rc shr rcShift) >= 0:
      scanBlack(s, desc, j)
      # XXX this should be done according to Lins' paper but currently breaks
      #when useJumpStack:
      #  s.setColor colPurple
    else:
      when useJumpStack:
        # first we have to repair all the nodes we have seen
        # that are still alive; we also need to mark what they
        # refer to as alive:
        while j.jumpStack.len > 0:
          let (entry, desc) = j.jumpStack.pop
          let t = head entry[]
          # not in jump stack anymore!
          t.rc = t.rc and not jumpStackFlag
          if t.color == colGray and (t.rc shr rcShift) >= 0:
            scanBlack(t, desc, j)
            # XXX this should be done according to Lins' paper but currently breaks
            #t.setColor colPurple
            when traceCollector:
              cprintf("[jump stack] %p %ld\n", t, t.rc shr rcShift)

      orcAssert(j.traceStack.len == 0, "scan: trace stack not empty")
      s.setColor(colWhite)
      trace(s, desc, j)
      while j.traceStack.len > 0:
        let (entry, desc) = j.traceStack.pop()
        let t = head entry[]
        if t.color == colGray:
          if (t.rc shr rcShift) >= 0:
            scanBlack(t, desc, j)
          else:
            when useJumpStack:
              # first we have to repair all the nodes we have seen
              # that are still alive; we also need to mark what they
              # refer to as alive:
              while j.jumpStack.len > 0:
                let (entry, desc) = j.jumpStack.pop
                let t = head entry[]
                # not in jump stack anymore!
                t.rc = t.rc and not jumpStackFlag
                if t.color == colGray and (t.rc shr rcShift) >= 0:
                  scanBlack(t, desc, j)
                  # XXX this should be done according to Lins' paper but currently breaks
                  #t.setColor colPurple
                  when traceCollector:
                    cprintf("[jump stack] %p %ld\n", t, t.rc shr rcShift)

            t.setColor(colWhite)
            trace(t, desc, j)

when false:
  proc writeCell(msg: cstring; s: Cell) =
    cfprintf(cstderr, "%s %p root index: %ld; RC: %ld; color: %ld\n",
      msg, s, s.rootIdx, s.rc shr rcShift, s.color)

proc collectColor(s: Cell; desc: PNimTypeV2; col: int; j: var GcEnv) =
  #[
    was: 'collectWhite'.

  proc collectWhite(s: Cell) =
    if s.color == colWhite and not buffered(s):
      s.setColor(colBlack)
      for t in sons(s):
        collectWhite(t)
      free(s) # watch out, a bug here!
  ]#
  if s.color == col and s.rootIdx == 0:
    orcAssert(j.traceStack.len == 0, "collectWhite: trace stack not empty")

    s.setColor(colBlack)
    j.toFree.add(s, desc)
    trace(s, desc, j)
    while j.traceStack.len > 0:
      let (entry, desc) = j.traceStack.pop()
      let t = head entry[]
      entry[] = nil # ensure that the destructor does touch moribund objects!
      if t.color == col and t.rootIdx == 0:
        j.toFree.add(t, desc)
        t.setColor(colBlack)
        trace(t, desc, j)

proc collectCyclesBacon(j: var GcEnv; lowMark: int) =
  # pretty direct translation from
  # https://researcher.watson.ibm.com/researcher/files/us-bacon/Bacon01Concurrent.pdf
  # Fig. 2. Synchronous Cycle Collection
  #[
    for s in roots:
      markGray(s)
    for s in roots:
      scan(s)
    for s in roots:
      remove s from roots
      s.buffered = false
      collectWhite(s)
  ]#
  let last = roots.len - 1
  when logOrc:
    for i in countdown(last, lowMark):
      writeCell("root", roots.d[i][0], roots.d[i][1])

  for i in countdown(last, lowMark):
    markGray(roots.d[i][0], roots.d[i][1], j)

  var colToCollect = colWhite
  if j.rcSum == j.edges:
    # short-cut: we know everything is garbage:
    colToCollect = colGray
    # remember the fact that we got so lucky:
    j.keepThreshold = true
  else:
    for i in countdown(last, lowMark):
      scan(roots.d[i][0], roots.d[i][1], j)

  for i in 0 ..< roots.len:
    let s = roots.d[i][0]
    s.rootIdx = 0
    collectColor(s, roots.d[i][1], colToCollect, j)

  inc j.freed, j.toFree.len

const
  defaultThreshold = when defined(nimFixedOrc): 10_000 else: 128

when defined(nimStressOrc):
  const rootsThreshold = 10 # broken with -d:nimStressOrc: 10 and for havlak iterations 1..8
else:
  var rootsThreshold = defaultThreshold

var collectorLock {.threadvar.}: bool
  ## prevents infinite recursion of the cycle collector, by disabling
  ## the collector when a collection is already in progress

proc freeCells(j: var GcEnv) =
  ## Destroys and frees all garbage cells and then deinits the list. This
  ## might register new cycle roots, so the cycle collector is disabled during
  ## the process, in order to prevent recursion and stack overflows.
  collectorLock = true
  for i in 0 ..< j.toFree.len:
    free(j.toFree.d[i][0], j.toFree.d[i][1])
  collectorLock = false

proc partialCollect(lowMark: int) =
  if collectorLock:
    return
  when false:
    if roots.len < 10 + lowMark: return
  when logOrc:
    cfprintf(cstderr, "[partialCollect] begin\n")
  var j: GcEnv
  init j.traceStack
  init j.toFree
  collectCyclesBacon(j, lowMark)
  when logOrc:
    cfprintf(cstderr, "[partialCollect] end; freed %ld touched: %ld work: %ld\n", j.freed, j.touched,
      roots.len - lowMark)
  roots.len = lowMark
  deinit j.traceStack
  # free the garbage cells after the root list is updated
  freeCells(j)
  deinit j.toFree

proc collectCycles() =
  ## Collect cycles.
  if collectorLock:
    return
  when logOrc:
    cfprintf(cstderr, "[collectCycles] begin\n")

  var j: GcEnv
  init j.traceStack
  init j.toFree
  when useJumpStack:
    init j.jumpStack

  # destroying garbage cells can lead to the registration of new cycles, so
  # cycle collection is run until no new roots are registered during cleanup
  while roots.len > 0:
    # reset all counters and lists:
    j.keepThreshold = false
    j.edges = 0
    j.rcSum = 0
    j.touched = 0
    j.freed = 0
    j.toFree.len = 0
    j.traceStack.len = 0

    when useJumpStack:
      j.jumpStack.len = 0
      collectCyclesBacon(j, 0)
      while j.jumpStack.len > 0:
        let (t, desc) = j.jumpStack.pop
        # not in jump stack anymore!
        t.rc = t.rc and not jumpStackFlag
    else:
      collectCyclesBacon(j, 0)

    # prepare for ``freeCell`` potentially registering new roots
    roots.len = 0
    # free the garbage cells after the root list is reset
    freeCells(j)

  # free the temporary lists allocated for the cycle collector:
  when useJumpStack:
    deinit j.jumpStack
  deinit j.toFree
  deinit j.traceStack
  # also free the global root list:
  deinit roots

  when not defined(nimStressOrc):
    # compute the threshold based on the previous history
    # of the cycle collector's effectiveness:
    # we're effective when we collected 50% or more of the nodes
    # we touched. If we're effective, we can reset the threshold:
    if j.keepThreshold and rootsThreshold <= defaultThreshold:
      discard
    elif j.freed * 2 >= j.touched:
      when not defined(nimFixedOrc):
        rootsThreshold = max(rootsThreshold div 3 * 2, 16)
      else:
        rootsThreshold = defaultThreshold
      #cfprintf(cstderr, "[collectCycles] freed %ld, touched %ld new threshold %ld\n", j.freed, j.touched, rootsThreshold)
    elif rootsThreshold < high(int) div 4:
      rootsThreshold = rootsThreshold * 3 div 2
  when logOrc:
    cfprintf(cstderr, "[collectCycles] end; freed %ld new threshold %ld touched: %ld mem: %ld rcSum: %ld edges: %ld\n", j.freed, rootsThreshold, j.touched,
      getOccupiedMem(), j.rcSum, j.edges)

proc registerCycle(s: Cell; desc: PNimTypeV2) =
  s.rootIdx = roots.len+1
  if roots.d == nil: init(roots)
  add(roots, s, desc)

  if roots.len >= rootsThreshold:
    collectCycles()
  when logOrc:
    writeCell("[added root]", s, desc)

proc GC_runOrc* =
  ## Forces a cycle collection pass.
  collectCycles()
  orcAssert roots.len == 0, "roots not empty!"

proc GC_enableOrc*() =
  ## Enables the cycle collector subsystem of `--gc:orc`. This is a `--gc:orc`
  ## specific API. Check with `when defined(gcOrc)` for its existence.
  when not defined(nimStressOrc):
    rootsThreshold = defaultThreshold

proc GC_disableOrc*() =
  ## Disables the cycle collector subsystem of `--gc:orc`. This is a `--gc:orc`
  ## specific API. Check with `when defined(gcOrc)` for its existence.
  when not defined(nimStressOrc):
    rootsThreshold = high(int)

proc GC_prepareOrc*(): int {.inline.} = roots.len

proc GC_partialCollect*(limit: int) =
  partialCollect(limit)

proc GC_fullCollect*() {.raises: [].} =
  ## Forces a full garbage collection pass. With `--gc:orc` triggers the cycle
  ## collector. This is an alias for `GC_runOrc`.
  collectCycles()

proc GC_enableMarkAndSweep*() =
  ## For `--gc:orc` an alias for `GC_enableOrc`.
  GC_enableOrc()

proc GC_disableMarkAndSweep*() =
  ## For `--gc:orc` an alias for `GC_disableOrc`.
  GC_disableOrc()

const
  acyclicFlag = 1 # see also cggtypes.nim, proc genTypeInfoV2Impl

when optimizedOrc:
  template markedAsCyclic(s: Cell; desc: PNimTypeV2): bool =
    (desc.flags and acyclicFlag) == 0 and (s.rc and maybeCycle) != 0
else:
  template markedAsCyclic(s: Cell; desc: PNimTypeV2): bool =
    (desc.flags and acyclicFlag) == 0

proc rememberCycle(isDestroyAction: bool; s: Cell; desc: PNimTypeV2) {.noinline.} =
  if isDestroyAction:
    if s.rootIdx > 0:
      unregisterCycle(s)
  else:
    # do not call 'rememberCycle' again unless this cell
    # got an 'incRef' event:
    if s.rootIdx == 0 and markedAsCyclic(s, desc):
      s.setColor colBlack
      registerCycle(s, desc)

proc nimDecRefIsLastCyclicDyn(p: pointer): bool {.compilerRtl, inl.} =
  if p != nil:
    var cell = head(p)
    if (cell.rc and not rcMask) == 0:
      result = true
      #cprintf("[DESTROY] %p\n", p)
    else:
      dec cell.rc, rcIncrement
    #if cell.color == colPurple:
    rememberCycle(result, cell, cast[ptr PNimTypeV2](p)[])

proc nimDecRefIsLastCyclicStatic(p: pointer; desc: PNimTypeV2): bool {.compilerRtl, inl.} =
  if p != nil:
    var cell = head(p)
    if (cell.rc and not rcMask) == 0:
      result = true
      #cprintf("[DESTROY] %p %s\n", p, desc.name)
    else:
      dec cell.rc, rcIncrement
    #if cell.color == colPurple:
    rememberCycle(result, cell, desc)
