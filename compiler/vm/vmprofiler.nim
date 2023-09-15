import
  std/[
    algorithm,
    times,
    strutils,
    tables
  ],
  compiler/ast/[
    lineinfos,
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/vm/[
    vmdef
  ]


proc enter*(prof: var Profiler, c: TCtx, sframe: StackFrameIndex) {.inline.} =
  if optProfileVM in c.config.globalOptions:
    prof.tEnter = cpuTime()
    prof.sframe = sframe

proc leaveImpl(prof: var Profiler, c: TCtx) {.noinline.} =
  let tLeave = cpuTime()
  var frameIdx = prof.sframe
  while frameIdx >= 0:
    let frame = c.sframes[frameIdx]
    if frame.prc != nil:
      let li = (frame.prc.info.fileIndex, frame.prc.info.line)
      if li notin prof.data:
        prof.data[li] = ProfileInfo()
      prof.data[li].time += tLeave - prof.tEnter
      if frameIdx == prof.sframe:
        inc prof.data[li].count
    dec frameIdx

proc leave*(prof: var Profiler, c: TCtx) {.inline.} =
  if optProfileVM in c.config.globalOptions:
    leaveImpl(prof, c)

proc dump*(conf: ConfigRef, prof: Profiler): string =
  ## Constructs a string containing a report of VM execution based on the
  ## data collected by `prof`. The report is formatted and ready to print to
  ## console or similar interface.
  const MaxEntries = 32
  var entries: seq[(SourceLinePosition, ProfileInfo)]

  proc compare(a: auto, b: ProfileInfo): int =
    let t1 = a[1].time
    if   t1 > b.time: -1
    elif t1 < b.time: +1
    else:              0

  # collect the entries with the most time spent:
  for line, info in prof.data.pairs:
    let pos = lowerBound(entries, info, compare)
    if pos < MaxEntries:
      entries.insert((line, info), pos)
      # discard excess entries:
      entries.setLen(max(entries.len, MaxEntries))

  # render the entries to a string:
  result = "prof:     µs    #instr  location\n"
  for (pos, info) in entries.items:
    result.add "  "
    result.add align($int(info.time * 1e6), 10)
    result.add align($int(info.count), 10)
    result.add "  "
    result.add toMsgFilename(conf, pos.fileIndex)
    result.add "\n"
