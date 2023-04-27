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
  var data = c.config.vmProfileData.data
  while frameIdx >= 0:
    let frame = c.sframes[frameIdx]
    if frame.prc != nil:
      let li = frame.prc.info
      if li notin data:
        data[li] = ProfileInfo()
      data[li].time += tLeave - prof.tEnter
      if frameIdx == prof.sframe:
        inc data[li].count
    dec frameIdx

proc leave*(prof: var Profiler, c: TCtx) {.inline.} =
  if optProfileVM in c.config.globalOptions:
    leaveImpl(prof, c)

proc dump*(conf: ConfigRef, pd: ProfileData): string =
  ## constructs a string containing a report of vm exectuion based on the given
  ## `ProfileData`. The report is formatted and ready to print to console or
  ## similar interface.
  var data = pd.data
  result = "\nprof:     µs    #instr  location\n"
  for i in 0..<32:
    var infoMax: ProfileInfo
    var flMax: TLineInfo
    for fl, info in data:
      if info.time > infoMax.time:
        infoMax = info
        flMax = fl
    if infoMax.count == 0:
      break
    result.add  "  " & align($int(infoMax.time * 1e6), 10) &
                       align($int(infoMax.count), 10) & "  " &
                       conf.toFileLineCol(flMax) & "\n"
    data.del flMax
