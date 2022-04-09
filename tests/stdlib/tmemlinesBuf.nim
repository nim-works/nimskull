discard """
targets: "!js"
"""

import memfiles
var inp = memfiles.open("tests/stdlib/tmemlinesBuf.nim")
var buffer: string = ""
var lineCount = 0
for line in lines(inp, buffer):
  lineCount += 1

close(inp)
doAssert lineCount == 13, $lineCount # this file's number of lines
