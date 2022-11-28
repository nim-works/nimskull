discard """
  description: "The program must exit with the code passed to `quit`"
  output: "quit called"
  exitcode: 1

  targets: "c vm js"
"""

quit("quit called", 1)

doAssert false # must not be reached