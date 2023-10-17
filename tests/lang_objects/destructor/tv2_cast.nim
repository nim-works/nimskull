discard """
  output: '''@[1]
@[116, 101, 115, 116]
@[1953719668, 875770417]
destroying O1'''
  cmd: '''nim c --gc:arc --expandArc:main --expandArc:main1 --expandArc:main2 --expandArc:main3 --hints:off --assertions:off $file'''
  nimout: '''--expandArc: main
var data
var :aux_2
var :aux_3
try:
  :aux_3 = encode(
      :aux_2 = newString(100)
      cast[seq[byte]](:aux_2))
  var :aux_4 = cast[string](:aux_3)
  =copy(data, :aux_4)
finally:
  =destroy(:aux_3)
  =destroy_1(:aux_2)
  =destroy_1(data)
-- end of expandArc ------------------------
--expandArc: main1
var s
var data
var :aux_3
try:
  s = newString(100)
  :aux_3 = encode(toOpenArrayByte(s, 0, -(len(s), 1)))
  var :aux_4 = cast[string](:aux_3)
  =copy(data, :aux_4)
finally:
  =destroy(:aux_3)
  =destroy_1(data)
  =destroy_1(s)
-- end of expandArc ------------------------
--expandArc: main2
var s
var data
var :aux_3
try:
  s = newSeq(100)
  :aux_3 = encode(s)
  var :aux_4 = cast[string](:aux_3)
  =copy(data, :aux_4)
finally:
  =destroy(:aux_3)
  =destroy_1(data)
  =destroy(s)
-- end of expandArc ------------------------
--expandArc: main3
var data
var :aux_2
var :aux_3
try:
  :aux_3 = encode(
      :aux_2 = newSeq(100)
      :aux_2)
  var :aux_4 = cast[string](:aux_3)
  =copy(data, :aux_4)
finally:
  =destroy(:aux_3)
  =destroy(:aux_2)
  =destroy_1(data)
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
