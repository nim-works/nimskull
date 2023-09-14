import
  std/[
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
  var data = prof.data
  result = "\nprof:     µs    #instr  location\n"
  for i in 0..<32:
    var infoMax: ProfileInfo
    var flMax: SourceLinePosition
    for fl, info in data:
      if info.time > infoMax.time:
        infoMax = info
        flMax = fl
    if infoMax.count == 0:
      break
    result.add  "  " & align($int(infoMax.time * 1e6), 10) &
                       align($int(infoMax.count), 10) & "  " &
                       toMsgFilename(conf, newLineInfo(flMax.fileIndex,
                                                       flMax.line.int,
                                                       -1)) &
                       "\n"
    data.del flMax
