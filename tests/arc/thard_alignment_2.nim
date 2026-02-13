discard """
  description: '''
    Tests for making sure the content of refs, seqs, and closures is
    properly aligned.
  '''
  matrix: "--gc:arc"
  output: "y"
"""

proc isAlignedCheck(p: pointer, alignment: int) =
  doAssert (cast[uint](p) and uint(alignment - 1)) == 0

proc isAlignedCheck[T](p: ref T, alignment: int) =
  isAlignedCheck(cast[pointer](p), alignment)

type
  MyAligned = object of RootObj
    a{.align: 128.}: float


var f: MyAligned
isAlignedCheck(f.addr, MyAligned.alignOf)

var fref = new(MyAligned)
isAlignedCheck(fref, MyAligned.alignOf)

var fs: seq[MyAligned]
var fr: seq[RootRef]

for i in 0..1000:
  fs.add MyAligned()
  isAlignedCheck(fs[^1].addr, MyAligned.alignOf)
  fs[^1].a = i.float

  fr.add new(MyAligned)
  isAlignedCheck(fr[^1], MyAligned.alignOf)
  ((ref MyAligned)fr[^1])[].a = i.float

for i in 0..1000:
  doAssert(fs[i].a == i.float)
  doAssert(((ref MyAligned)fr[i]).a == i.float)


proc lambdaTest2(a: MyAligned, z: ref MyAligned): auto =
  var x1: MyAligned
  x1.a = a.a + z.a
  var x2: MyAligned
  x2.a = a.a - z.a
  let capturingLambda = proc(x: MyAligned): MyAligned =
    var cc: MyAligned
    var bb: MyAligned
    isAlignedCheck(x1.addr, MyAligned.alignOf)
    isAlignedCheck(x2.addr, MyAligned.alignOf)
    isAlignedCheck(cc.addr, MyAligned.alignOf)
    isAlignedCheck(bb.addr, MyAligned.alignOf)
    isAlignedCheck(z, MyAligned.alignOf)

    cc.a = x1.a + x1.a + z.a
    bb.a = x2.a - z.a

    isAlignedCheck(result.addr, MyAligned.alignOf)
    result.a = cc.a + bb.a + x2.a

  return capturingLambda


let q1 = lambdaTest2(MyAligned(a: 1.0), (ref MyAligned)(a: 2.0))
let q2 = lambdaTest2(MyAligned( a: -1.0), (ref MyAligned)(a: -2.0))

isAlignedCheck(rawEnv(q1), MyAligned.alignOf)
isAlignedCheck(rawEnv(q2), MyAligned.alignOf)
discard q1(MyAligned(a: 1.0))
discard q2(MyAligned(a: -1.0))


#-----------------------------------------------------------------------------

block:
  var s: seq[seq[MyAligned]]
  for len in 0..128:
    s.add newSeq[MyAligned](len)
    for i in 0..<len:
      s[^1][i] = MyAligned(a: 1.0)

    if len > 0:
      isAlignedCheck(s[^1][0].addr, MyAligned.alignOf)

echo "y"
