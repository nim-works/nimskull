discard """
outputsub: ""
"""

import memfiles
var inp = memfiles.open("tests/stdlib/os/tmemlines.nim")
for line in lines(inp):
  echo("#" & line & "#")
close(inp)
