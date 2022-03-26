discard """
  output: '''
verstuff
'''
  matrix: "--gc:arc"
"""

# bug #13622

import strformat

template necho*(args: string) {.dirty.} =
  if getCurrentException() != nil:
    echo args
  else:
    stdout.writeLine(args)

proc main(cond: bool; arg: string) =
  if cond:
    necho &"ver{arg}\n"

main(true, "stuff")
