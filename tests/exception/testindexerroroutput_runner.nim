discard """
  description: "test runner for testindexerroroutput.nims"
  joinable: false
  target: native
"""

# TODO: remove this once testament has first class support script tests

import std/[os, osproc, strutils]

const
  nim = getCurrentCompilerExe()
  cmdPrefix = nim & " e tests/exception/testindexerroroutput.nims test"
for i in 1..4:
  let
    (outp, errC) = execCmdEx(cmdPrefix & $i, {poStdErrToStdOut})
    expected = "index 3 not in 0 .. 2"
  doAssert errC != 0, "test:$#\nexitCode: $#\noutput:\n$#" % [$i, $errC, outp]
  doAssert outp.contains expected:
    "test:$#\noutput:\n$#\nexpected:\n$#\n" % [$i, outp, expected]