discard """
  targets: "cpp"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/10231
      regression caused by WEXITSTATUS: nim cpp compiler/nim.nim fails on OSX
    . Probably caused by https://github.com/nim-lang/Nim/pull/10222
    . https://github.com/nim-lang/Nim/pull/10274
      execShellCmd now returns nonzero when child killed with signal
  '''
"""

import os

# consider moving this inside tosproc (taking care that it's for cpp mode)

if paramCount() == 0:
  # main process
  doAssert execShellCmd(getAppFilename().quoteShell & " test") == 1
else:
  quit 1

