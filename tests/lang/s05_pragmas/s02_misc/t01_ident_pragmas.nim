discard """
description: '''
Test pragma annotations that can be used on identifier declaratios.
'''

cmd: "nim c -r -d:strdefineUsed='test' -d:intdefineUsed=2 -d:booldefineUsed=false $options $file"

"""

## It is possible to put pragma annotations on different variable declarations.

block const_define:
  const strdefineDefault {.strdefine.} = "default value"
  const strdefineUsed {.strdefine.} = "default value"

  doAssert strdefineDefault == "default value"
  doAssert strdefineUsed == "test"

  const intdefineDefault {.intdefine.} = 12
  const intdefineUsed {.intdefine.} = 12

  doAssert intdefineDefault == 12
  doAssert intdefineUsed == 2

  const booldefineDefault {.booldefine.} = true
  const booldefineUsed {.booldefine.} = true

  doAssert booldefineDefault == true
  doAssert booldefineUsed == false
