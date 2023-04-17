discard """
description: "test runner for tnimscriptwithmacro"
joinable: false
target: native
"""

# TODO: remove this once testament has first class support script tests

import std/[os, osproc, strutils]

const expected = "foobar\nnothing\nhallo"

let (outp, code) = execCmdEx(getCurrentCompilerExe() &
                             " tests/tools/tnimscriptwithmacro.nims")

doAssert code == 0, "non-zero exitcode ($#), output:\n$#" % [$code, outp]
doAssert outp.contains expected:
  "exected:$#\ngot:\n$#" % [expected, outp]