discard """
  output: '''
Hithere, what's your name?Hathere, what's your name?
fA13msg1falsefB14msg2truefC15msg3false
@[()]
@[1, 42, 3]
@[1, 42, 3]
2345623456
'''
"""

block tseq2:
  proc `*`(a, b: seq[int]): seq[int] =
    # allocate a new sequence:
    newSeq(result, len(a))
    # multiply two int sequences:
    for i in 0..len(a)-1: result[i] = a[i] * b[i]

  doAssert(@[1, 2, 3] * @[1, 2, 3] == @[1, 4, 9])



block tseqcon:
  const nestedFixed = true

  type
    TRec {.final.} = object
      x, y: int
      s: string
      seq: seq[string]
    TRecSeq = seq[TRec]

  proc test() =
    var s, b: seq[string]
    s = @[]
    add(s, "Hi")
    add(s, "there, ")
    add(s, "what's your name?")

    b = s # deep copying here!
    b[0][1] = 'a'

    var output = ""
    for i in 0 .. len(s)-1:
      output.add s[i]
    for i in 0 .. len(b)-1:
      output.add b[i]
    echo output

  when nestedFixed:
    proc nested() =
      var
        s: seq[seq[string]]
      for i in 0..10_000: # test if the garbage collector
        # now works with sequences
        s = @[
          @["A", "B", "C", "D"],
          @["E", "F", "G", "H"],
          @["I", "J", "K", "L"],
          @["M", "N", "O", "P"]]

  test()
  when nestedFixed:
    nested()


import os
block tseqcon2:
  proc rec_dir(dir: string): seq[string] =
    result = @[]
    for kind, path in walk_dir(dir):
      if kind == pcDir:
        add(result, rec_dir(path))
      else:
        add(result, path)



block tseqtuple:
  type
    TMsg = tuple[
      file: string,
      line: int,
      msg: string,
      err: bool]

  var s: seq[TMsg] = @[]

  s.add(("fA", 13, "msg1", false))
  s.add(("fB", 14, "msg2", true))
  s.add(("fC", 15, "msg3", false))

  var output = ""
  for file, line, msg, err in items(s):
    output.add(file)
    output.add($line)
    output.add(msg)
    output.add($err)
  echo output, ""


import sequtils
block tsequtils:
  proc testFindWhere(item : int) : bool =
    if item != 1: return true

  var seq1: seq[int] = @[]

  seq1.add(1)
  seq1.add(3)
  seq1.add(5)
  seq1.add(7)

  var seq2: seq[int] = @[2, 4, 6]
  var final = zip(seq1, seq2)

  doAssert final == @[(1, 2), (3, 4), (5, 6)], "Zip mismatch, got: " & $final

  # echo "Zip: ", $$(final)

  #Test findWhere as a iterator

  var itms: string
  for itm in filter(seq1, testFindWhere):
    # echo "Filter Iterator: ", $$(itm)
    itms.add $itm
  
  doAssert itms == "357", "Filter Iterator mismatch, got: " & $itms


  #Test findWhere as a proc

  var fullseq: seq[int] = filter(seq1, testFindWhere)

  # echo "Filter: ", $$(fullseq)
  doAssert fullSeq == @[3, 5, 7], "Filter mismatch, got: " & $fullSeq


  #Test findIt as a template

  var finditval: seq[int] = filterIt(seq1, it!=5)

  # echo "FilterIt: ", $$(finditval)
  doAssert finditval == @[1, 3, 7], "FilterIt mismatch, got: " & $finditval

  var concatseq = concat(seq1,seq2)
  # echo "Concat: ", $$(concatseq)
  doAssert concatseq == @[1, 3, 5, 7, 2, 4, 6],
    "Concat mismatch, got: " & $finditval

  var seq3 = @[1,2,3,4,5,5,5,7]
  var dedupseq = deduplicate(seq3)
  # echo "Deduplicate: ", $$(dedupseq)
  doAssert dedupseq == @[1, 2, 3, 4, 5, 7], "Deduplicate mismatch, got: " & $dedupseq
  
  # bug #4973
  type
    SomeObj = object
    OtherObj = object
      field: SomeObj

  let aSeq = @[OtherObj(field: SomeObj())]
  let someObjSeq = aSeq.mapIt(it.field)
  echo someObjSeq



block tshallowseq:
  proc xxx() =
    var x: seq[int] = @[1, 2, 3]
    var y: seq[int]
    system.shallowCopy(y, x)
    y[1] = 42
    echo y
    echo x
  xxx()


block tshallowemptyseq:
  proc test() =
    var nilSeq: seq[int] = @[]
    var emptySeq: seq[int] = newSeq[int]()
    block:
      var t = @[1,2,3]
      shallow(nilSeq)
      t = nilSeq
      doAssert t == @[]
    block:
      var t = @[1,2,3]
      shallow(emptySeq)
      t = emptySeq
      doAssert t == @[]
    block:
      var t = @[1,2,3]
      shallowCopy(t, nilSeq)
      doAssert t == @[]
    block:
      var t = @[1,2,3]
      shallowCopy(t, emptySeq)
      doAssert t == @[]
  test()


import strutils
block ttoseq:
  var output = ""
  for x in toSeq(countup(2, 6)):
    output.add($x)
  for x in items(toSeq(countup(2, 6))):
    output.add($x)
  var y: typeof("a b c".split)
  y = "xzy"
  echo output

block tseqmapitchain:
  doAssert @[101, 102] == [1, 2].mapIt(func (x: int): int = it + x).mapIt(it(100))


for i in 0..100:
  # fix #14655
  var test = newSeqOfCap[uint32](1)
  test.setLen(1)
  doAssert test[0] == 0, $(test[0], i)
