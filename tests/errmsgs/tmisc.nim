discard """
cmd: "nim check $file"
action: reject
nimout: '''
tmisc.nim(24, 13) Error: object construction uses ':', not '='
tmisc.nim(26, 5) Error: wrong number of arguments
tmisc.nim(30, 9) Error: expression has no type: foo
tmisc.nim(38, 5) Error: type mismatch: got <int, uint>
but expected one of:
template `.=`(a: int; b: untyped; c: int)
  first type mismatch at position: 3
  required type for c: int
  but expression 'c' is of type: uint
template b=(a: int; c: int)
  first type mismatch at position: 2
  required type for c: int
  but expression 'c' is of type: uint
'''
"""

type O = object
  f: int

discard O(f = 1)

`is`(1, int, int)

macro foo = discard

let x = foo

template `b=`(a: int, c: int) = discard
template `.=`(a: int, b: untyped, c: int) = discard

let a = 1
let c = 3.uint

a.b = c

