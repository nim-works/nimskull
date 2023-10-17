## Implements a simple time profiler for the VM.

import
  std/[
    algorithm,
    hashes,
    times,
    strutils,
    tables
  ],
  compiler/ast/[
    ast_types,
    lineinfos
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/vm/[
    vmdef
  ]

from compiler/ast/ast import id

func hash(s: PSym): Hash {.inline.} =
  hash(s.id)

proc enter*(prof: var Profiler) {.inline.} =
  if prof.enabled:
    prof.tEnter = cpuTime()

proc leaveImpl(prof: var Profiler, frames: openArray[TStackFrame]) {.noinline.} =
  # note: the implementation is kept in a separate noinline procedure in
  # order to reduce the instruction-cache pressure when profiling is disabled
  let diff = cpuTime() - prof.tEnter

  for i in 0..<frames.len:
    let prc = frames[i].prc
    if prc != nil:
      # ensure that an entry exists:
      let data = addr prof.data.mgetOrPut(prc, ProfileInfo())
      # update the time spent within the procedure:
      data.time += diff
      # for the active frame, increment the number of samples taken
      if i == frames.high:
        inc data.count

proc leave*(prof: var Profiler, frames: openArray[TStackFrame]) {.inline.} =
  ## If profiling is enabled, ends a measurement, updating the collected data.
  ## The data of the profiler entries associated with the `frames` is updated
  ## with the measured time, and the "number of samples" counter of the current
  ## active frame (last entry in the list) is incremented by one.
  if prof.enabled:
    leaveImpl(prof, frames)

proc dump*(conf: ConfigRef, prof: Profiler): string =
  ## Constructs a string containing a report of VM execution based on the given
  ## `prof`. The report is formatted and ready to print to console or
  ## similar interface.
  const MaxEntries = 32
  var entries: seq[(TLineInfo, ProfileInfo)]

  proc compare(a: auto, b: ProfileInfo): int =
    let t1 = a[1].time
    if   t1 > b.time: -1
    elif t1 < b.time: +1
    else:              0

  # collect the entries with the most time spent:
  for sym, info in prof.data.pairs:
    let pos = lowerBound(entries, info, compare)
    if pos < MaxEntries:
      entries.insert((sym.info, info), pos)
      # discard excess entries:
      entries.setLen(min(entries.len, MaxEntries))

  # render the entries to a string:
  result = "prof:     µs    #instr  location\n"
  for (pos, info) in entries.items:
    result.add "  "
    result.add align($int(info.time * 1e6), 10)
    result.add align($int(info.count), 10)
    result.add "  "
    result.add toFileLineCol(conf, pos)
    result.add "\n"
