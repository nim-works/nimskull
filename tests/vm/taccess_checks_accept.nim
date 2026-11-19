discard """
  description: '''Tests to ensure that valid usages aren't reported as an
                  access violation'''
  action: compile
"""

import maccess_checks

type
  Distinct1 = distinct int
  Distinct2 = distinct int
  Tuple = tuple[x: int, y: float]


static:
  # no location is accessed here, so no error is reported
  discard addr(localPtr(Object).a)

static:
  # the VM doesn't differentiate between distinct/non-distinct
  var a = Distinct1(1)
  let b = a.asPtr(Distinct2)
  doAssert int(b[]) == 1
  b[] = Distinct2(2)
  doAssert int(a) == 2

static:
  # the VM doesn't differentiate between named tuples and unnamed tuples
  block:
    var a: Tuple = (1, 2.0)
    let b = a.asPtr((int, float))
    doAssert b[][0] == 1
    doAssert b[][1] == 2.0
    b[][0] = 3
    b[][1] = 4.0
    doAssert a.x == 3
    doAssert a.y == 4.0

  block:
    var a = (1, 2.0)
    let b = a.asPtr(Tuple)
    doAssert b.x == 1
    doAssert b.y == 2.0
    b.x = 3
    b.y = 4.0
    doAssert a[0] == 3
    doAssert a[1] == 4.0


objectTest((int32, string)): # partial unnamed tuple overlay
  doAssert p[][0] == 3
  # due to alignment of `string`, this works
  doAssert p[][1] == "c"

objectTest(tuple[x: int32, y: string]): # named tuple overlay
  doAssert p.x == 3
  # due to alignment of `string`, this works
  doAssert p.y == "c"

objectTest((int32, bool, string, seq[int])): # full unnamed tuple overlay
  doAssert p[][0] == 3
  doAssert p[][1] == true
  doAssert p[][2] == "c"
  doAssert p[][3] == [1, 2]

static: # partial overlay in the middle of an object
  var o = Object(c: "c", d: @[1, 2])
  let p = o.c.asPtr(tuple[x: string, y: seq[int]])
  doAssert p.x == "c"
  doAssert p.y == [1, 2]

static: # aliasing char with uint8
  var a1 = "hi"
  var a2 = [0x68'u8]
  doAssert a1[0].asPtr(uint8)[] == 0x68
  doAssert a1[0].asPtr(int8)[] == 104
  doAssert a2[0].asPtr(char)[] == 'h'

static: # aliasing uintXX with intXX & vice versa
  var a1 = [0xFF'u8]
  var a2 = [-1'i8]
  doAssert a1[0].asPtr(int8)[] == -1'i8
  doAssert a2[0].asPtr(uint8)[] == 0xFF'u8

  var b1 = 0xCAFE'u16
  var b2 = -13570'i16
  doAssert b1.asPtr(int16)[] == -13570'i16
  doAssert b2.asPtr(uint16)[] == 0xCAFE'u16

  var c1 = 0x12345678'u32
  var c2 = 305419896'i32
  doAssert c1.asPtr(int32)[] == 305419896'i32
  doAssert c2.asPtr(uint32)[] == 0x12345678'u32

  var d1 = 0x9001900190019001'u64
  var d2 = -8070010720885829631'i64
  doAssert d1.asPtr(int64)[] == -8070010720885829631'i64
  doAssert d2.asPtr(uint64)[] == 0x9001900190019001'u64
