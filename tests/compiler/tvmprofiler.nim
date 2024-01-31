discard """
  description: '''
    Basic unit test for making sure that the VM profiler works as expected
  '''
  targets: native
"""

import
  std/[
    os,
    strscans,
    strutils
  ],
  compiler/ast/[
    ast,
    lineinfos
  ],
  compiler/front/[
    options
  ],
  compiler/utils/[
    pathutils
  ],
  compiler/vm/[
    vmdef,
    vmprofiler
  ]

var
  conf = newConfigRef(nil)
# get a ``FileInfoIdx``. The file's content doesn't matter, it only needs to
# exist
let self = conf.fileInfoIdx(currentSourcePath().AbsoluteFile)

conf.filenameOption = foName # render the filepath as only the name

# setup an enabled profiler:
var profiler = Profiler(enabled: true)

# setup some pseudo symbols to represent the procedures:
var syms: seq[PSym]
for i in 1..5:
  syms.add PSym(itemId: ItemId(module: 0, item: int32(i-1)),
                info: newLineInfo(self, i, i-1))

var frames: seq[TStackFrame]

# "enter" multiple procedures:
for it in syms.items:
  frames.add TStackFrame(prc: it)

# take a sample on each frame:
while frames.len > 0:
  profiler.enter()
  # sleep for a bit. The exact amount doesn't matter
  sleep(1)
  profiler.leave(frames)
  # "leave" the frame
  discard frames.pop()

# render the data and verify the output:
let output = dump(conf, profiler).splitLines()
# the output must contain a header + 5 entries + trailing newline
doAssert output.len == 7, $output
doAssert output[^1] == "", output[^1]

# verify the entries. Each must have exactly one sample taken, and the
# procedure with the most time spent must be the first symbol, followed by
# second one, etc.
for i in 1..<output.len - 1:
  var
    time, num, line, col: int
    path: string
  doAssert scanf(output[i], "$s$i$s$i$s$w.nim($i,$s$i)$s$.",
                 time, num, path, line, col)
  # since we do not mock the ``cpuTime`` procedure, we can only check whether
  # the value is within a reasonable range
  doAssert time in 1..300000, "time value is out-of-range: $#" % [$time]
  doAssert num == 1 # one sample is taken within each procedure
  doAssert path == "tvmprofiler"
  # check that the line + column are correct:
  doAssert line == i
  doAssert col == i
