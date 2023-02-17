discard """
  output: '''@[1]
@[116, 101, 115, 116]
@[1953719668, 875770417]
destroying O1'''
  cmd: '''nim c --gc:arc --expandArc:main --expandArc:main1 --expandArc:main2 --expandArc:main3 --hints:off --assertions:off $file'''
  nimout: '''--expandArc: main

var :tmp
var data
var :tmp_1
try:
  var :tmp_2 = encode do:
    var :tmp_3 = newString(100)
    :tmp = :tmp_3
    cast[[type node]](:tmp)
  :tmp_1 = :tmp_2
  var :tmp_4 = cast[string](:tmp_1)
  `=copy`(data, :tmp_4)
finally:
  `=destroy`(:tmp_1)
  `=destroy_1`(:tmp)
  `=destroy_1`(data)
-- end of expandArc ------------------------
--expandArc: main1

var s
var :tmp
var data
try:
  s = newString(100)
  var :tmp_1 = encode(toOpenArrayByte(s, 0, `-`(len(s), 1)))
  :tmp = :tmp_1
  var :tmp_2 = cast[string](:tmp)
  `=copy`(data, :tmp_2)
finally:
  `=destroy`(:tmp)
  `=destroy_1`(data)
  `=destroy_1`(s)
-- end of expandArc ------------------------
--expandArc: main2

var s
var :tmp
var data
try:
  s = newSeq(100)
  var :tmp_1 = encode(s)
  :tmp = :tmp_1
  var :tmp_2 = cast[string](:tmp)
  `=copy`(data, :tmp_2)
finally:
  `=destroy`(:tmp)
  `=destroy_1`(data)
  `=destroy`(s)
-- end of expandArc ------------------------
--expandArc: main3

var :tmp
var data
var :tmp_1
try:
  var :tmp_2 = encode do:
    var :tmp_3 = newSeq(100)
    :tmp = :tmp_3
    :tmp
  :tmp_1 = :tmp_2
  var :tmp_4 = cast[string](:tmp_1)
  `=copy`(data, :tmp_4)
finally:
  `=destroy`(:tmp_1)
  `=destroy`(:tmp)
  `=destroy_1`(data)
-- end of expandArc ------------------------'''
"""

func encode*(src: openArray[byte]): seq[byte] =
  result = newSeq[byte](src.len)

template compress*(src: string): string =
  cast[string](encode(cast[seq[byte]](src)))

proc main =
  let data = compress(newString(100))
main()

proc main1 =
  var
    s = newString(100)
  let data = cast[string](encode(s.toOpenArrayByte(0, s.len-1)))
main1()

proc main2 =
  var
    s = newSeq[byte](100)
  let data = cast[string](encode(s))
main2()

proc main3 =
  let data = cast[string](encode(newSeq[byte](100)))
main3()

# bug #11018
discard cast[seq[uint8]](@[1])
discard cast[seq[uint8]]("test")
echo cast[seq[uint8]](@[1])
echo cast[seq[uint8]]("test")

discard cast[string](@[116'u8, 101, 115, 116])
#echo cast[string](@[116'u8, 101, 115, 116, 0])
var a = cast[seq[uint32]]("test1234")
a.setLen(2)
echo a


#issue 11204
var ac {.compileTime.} = @["a", "b"]
const bc = ac.len


type
  O = object of RootRef
    i: int

  O1 = object of O
  O2 = object of O

proc `=destroy`(o: var O) =
  echo "destroying O"

proc `=destroy`(o: var O1) =
  echo "destroying O1"

proc `=destroy`(o: var O2) =
  echo "destroying O2"

proc test =
  let o3 = cast[ref O2]((ref O1)())

test()
