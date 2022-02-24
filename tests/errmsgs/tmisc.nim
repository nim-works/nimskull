discard """
cmd: "nim check $file"
action: reject
nimout: '''
tmisc.nim(14, 13) Error: object construction uses ':', not '='
tmisc.nim(16, 5) Error: wrong number of arguments
tmisc.nim(20, 9) Error: expression has no type: foo
'''
"""

type O = object
  f: int

discard O(f = 1)

`is`(1, int, int)

macro foo = discard

let x = foo
