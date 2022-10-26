discard """
labels: "identifier local overload proc resolution"
description: '''
. Test overloading of procs with locals
'''
"""

type
  TMyType = object
    len: int
    data: string

proc len(x: TMyType): int {.inline.} = return x.len
proc data(x: TMyType): string {.inline.} = return x.data

proc doLen(s: TMyType, len: int) : int =
  return len( s )
proc doData(s: TMyType, len: int) : string =
  return data( s )


var
  m: TMyType

const size = 7
const content = "1234"

m.len = size
m.data = content

doAssert doLen(m, 5) == size
doAssert doData(m, 5) == content
