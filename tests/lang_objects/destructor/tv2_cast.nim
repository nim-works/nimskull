discard """
  output: '''@[1]
@[116, 101, 115, 116]
@[1953719668, 875770417]
destroying O1'''
  cmd: '''nim c --gc:arc --expandArc:main --expandArc:main1 --expandArc:main2 --expandArc:main3 --hints:off --assertions:off $file'''
  nimout: '''--expandArc: main
scope:
  def _2: string = newString(arg 100)
  def_cursor _3: seq[byte] = cast _2
  def _4: openArray[byte] = toOpenArray _3
  def _5: seq[byte] = encode(arg _4) -> [L0, Resume]
  def_cursor _6: string = cast _5
  def data: string
  =copy(name data, arg _6)
  =destroy(name data)
  =destroy(name _5)
  goto [L0, L1]
  finally (L0):
    =destroy(name _2)
    continue {L1}
  L1:
-- end of expandArc ------------------------
--expandArc: main1
scope:
  def s: string = newString(arg 100)
  def_cursor _3: string = s
  def _4: int = lengthStr(arg _3)
  def _5: int = subI(arg _4, arg 1) -> [L0, Resume]
  chckBounds(arg s, arg 0, arg _5) -> [L0, Resume]
  def _6: openArray[byte] = toOpenArray s, 0, _5
  def _7: seq[byte] = encode(arg _6) -> [L0, Resume]
  def_cursor _8: string = cast _7
  def data: string
  =copy(name data, arg _8)
  =destroy(name data)
  =destroy(name _7)
  goto [L0, L1]
  finally (L0):
    =destroy(name s)
    continue {L1}
  L1:
-- end of expandArc ------------------------
--expandArc: main2
scope:
  def s: seq[byte] = newSeq(arg 100) -> [Resume]
  def _3: openArray[byte] = toOpenArray s
  def _4: seq[byte] = encode(arg _3) -> [L0, Resume]
  def_cursor _5: string = cast _4
  def data: string
  =copy(name data, arg _5)
  =destroy(name data)
  =destroy(name _4)
  goto [L0, L1]
  finally (L0):
    =destroy(name s)
    continue {L1}
  L1:
-- end of expandArc ------------------------
--expandArc: main3
scope:
  def _2: seq[byte] = newSeq(arg 100) -> [Resume]
  def _3: openArray[byte] = toOpenArray _2
  def _4: seq[byte] = encode(arg _3) -> [L0, Resume]
  def_cursor _5: string = cast _4
  def data: string
  =copy(name data, arg _5)
  =destroy(name data)
  =destroy(name _4)
  goto [L0, L1]
  finally (L0):
    =destroy(name _2)
    continue {L1}
  L1:
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
