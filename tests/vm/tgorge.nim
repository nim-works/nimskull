discard """
matrix: "--experimental:vmopsDanger"
disabled: "windows"
"""

# If your os is windows and this test fails for you locally, please
# check what is going wrong.

import os

template getScriptDir(): string =
  parentDir(instantiationInfo(-1, true).filename)

# See also simpler test in Nim/tests/vm/tvmops.nim for a simpler
# cross platform way.
block gorge:
  const
    execName = when defined(windows): "tgorge.bat" else: "./tgorge.sh"
    relOutput = gorge(execName)
    absOutput = gorge(getScriptDir() / execName)

  doAssert relOutput == "gorge test", relOutput
  doAssert absOutput == "gorge test", absOutput

block gorgeEx:
  const
    execName = when defined(windows): "tgorgeex.bat" else: "./tgorgeex.sh"
    res = gorgeEx(execName)
  doAssert res.output == "gorgeex test", res.output
  doAssert res.exitCode == 1, $res.exitCode
