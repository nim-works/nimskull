discard """
  output: '''@[1]
@[116, 101, 115, 116]
@[1953719668, 875770417]
destroying O1'''
  cmd: '''nim c --gc:arc --expandArc:main --expandArc:main1 --expandArc:main2 --expandArc:main3 --hints:off --assertions:off $file'''
  nimout: '''--expandArc: main
scope:
  try:
    def _0: string = newString(arg 100)
    def_cursor _1: seq[byte] = cast _0
    def_cursor _2: openArray[byte] = toOpenArray _1
    def _3: seq[byte] = encode(arg _2) (raises)
    def data: string
    def _4: string = cast _3
    =copy(name data, arg _4)
  finally:
    =destroy(name data)
    =destroy(name _3)
    =destroy(name _0)
-- end of expandArc ------------------------
--expandArc: main1
scope:
  try:
    def s: string = newString(arg 100)
    def_cursor _0: string = s
    def_cursor _1: int = lengthStr(arg _0)
    def_cursor _2: int = subI(arg _1, arg 1) (raises)
    chckBounds(arg s, arg 0, arg _2) (raises)
    def_cursor _3: openArray[byte] = toOpenArray s, 0, _2
    def _4: seq[byte] = encode(arg _3) (raises)
    def data: string
    def _5: string = cast _4
    =copy(name data, arg _5)
  finally:
    =destroy(name data)
    =destroy(name _4)
    =destroy(name s)
-- end of expandArc ------------------------
--expandArc: main2
scope:
  try:
    def s: seq[byte] = newSeq(arg 100) (raises)
    def_cursor _0: openArray[byte] = toOpenArray s
    def _1: seq[byte] = encode(arg _0) (raises)
    def data: string
    def _2: string = cast _1
    =copy(name data, arg _2)
  finally:
    =destroy(name data)
    =destroy(name _1)
    =destroy(name s)
-- end of expandArc ------------------------
--expandArc: main3
scope:
  try:
    def _0: seq[byte] = newSeq(arg 100) (raises)
    def_cursor _1: openArray[byte] = toOpenArray _0
    def _2: seq[byte] = encode(arg _1) (raises)
    def data: string
    def _3: string = cast _2
    =copy(name data, arg _3)
  finally:
    =destroy(name data)
    =destroy(name _2)
    =destroy(name _0)
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
