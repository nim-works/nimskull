discard """
ccodeCheck: "\\i @'NIM_ALIGN(128) NI mylocal1' .*"
labels: "pragma alignment generic"
description: '''
  . First one is is for Azure. The keyword ``alignof`` only exists in ``c++11``
    and newer. On Azure gcc does not default to c++11 yet.

  . From https://github.com/nim-lang/Nim/issues/13122
    {.align.} pragma is not applied if there is a generic field
'''
targets: "!js"
knownIssue.vm: "VM does not support casting a ptr to a uint"
"""

import globalalignas

var toplevel1 {.align: 32.} : int32
var toplevel2 {.align: 32.} : int32
var toplevel3 {.align: 32.} : int32

proc foobar() =
  var myvar1 {.global, align(64).}: int = 123
  var myvar2 {.global, align(64).}: int = 123
  var myvar3 {.global, align(64).}: int = 123

  doAssert (cast[uint](addr(myglobal1)) and 127) == 0
  doAssert (cast[uint](addr(myglobal2)) and 127) == 0
  doAssert (cast[uint](addr(myglobal3)) and 127) == 0

  doAssert (cast[uint](addr(myvar1)) and 63) == 0
  doAssert (cast[uint](addr(myvar2)) and 63) == 0
  doAssert (cast[uint](addr(myvar3)) and 63) == 0

  doAssert (cast[uint](addr(toplevel1)) and 31) == 0
  doAssert (cast[uint](addr(toplevel2)) and 31) == 0
  doAssert (cast[uint](addr(toplevel3)) and 31) == 0

  # test multiple align expressions
  var mylocal1 {.align(128), align(32).}: int = 123
  var mylocal2 {.align(128), align(32).}: int = 123
  var mylocal3 {.align(32), align(128).}: int = 123

  doAssert (cast[uint](addr(mylocal1)) and 127) == 0
  doAssert (cast[uint](addr(mylocal2)) and 127) == 0
  doAssert (cast[uint](addr(mylocal3)) and 127) == 0

foobar()

block issue_13122:

  type Bug[T] = object
    bug{.align:64.}: T
    sideffect{.align:64.}: int

  var bug: Bug[int]
  doAssert sizeof(bug) == 128, "Oops my size is " & $sizeof(bug) # 16
