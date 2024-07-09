discard """
  description: "Test packedsets bitmap container"
"""

import std/private/packedsets/bitmaps
import std/sequtils

block iterators:
  proc makeBitmap(values: varargs[uint16]): BitmapContainer =
    result = newBitmapContainer()
    for value in values.items:
      result.incl value

  block items:
    ## Tests for the items iterator
    const values = [0u16, 42, 66, 128, 191, 1000, 40000]
      ## These values are selected to span bitmap word boundaries and are
      ## positioned at potentially troublesome locations (ie. MSB/LSB)

    let bitmap = makeBitmap(values)
    let asSeq = bitmap.toSeq()
    doAssert asSeq == values:
      "Unexpected values: " & $asSeq

  block intersection:
    let b0 = makeBitmap(0, 42, 66, 128, 191)
    let b1 = makeBitmap(50, 66, 128, 191, 10000)

    const intersect = [66u16, 128, 191]
    let asSeq = toSeq(intersection(b0, b1))
    doAssert asSeq == intersect:
      "Unexpected values: " & $asSeq

  block difference:
    let b0 = makeBitmap(0, 42, 66, 128, 191)
    let b1 = makeBitmap(50, 66, 128, 191, 10000)

    const diff = [0u16, 42]
    let asSeq = toSeq(difference(b0, b1))
    doAssert asSeq == diff:
      "Unexpected values: " & $asSeq

  block symmetricDifference:
    let b0 = makeBitmap(0, 42, 66, 128, 191)
    let b1 = makeBitmap(50, 66, 128, 191, 10000)

    const diff = [0u16, 42, 50, 10000]
    let asSeq = toSeq(symmetricDifference(b0, b1))
    doAssert asSeq == diff:
      "Unexpected values: " & $asSeq

block basic:
  ## Basic property tests
  block unshare:
    ## Bitmap instances are not shared with one another
    var bitmap0 = newBitmapContainer()
    bitmap0.incl 42
    var bitmap2 = bitmap0
    bitmap0.excl 42
    doAssert 42 notin bitmap0
    doAssert 42 in bitmap2

  block comparison:
    block equality:
      ## Bitmap equality is dictated by its content
      let b0 = newBitmapContainer()
      var b1 = newBitmapContainer()
      let b2 = newBitmapContainer()

      b1.incl 0xdead

      doAssert b1 != b2:
        "Bitmap " & $b1 & " is equal to " & $b2 & ", but should not be"
      doAssert b0 == b2:
        "Two empty bitmaps is not equal to one another, but should be"

    block subset:
      ## Verify subset test operators
      var b0 = newBitmapContainer()
      b0.incl 1
      b0.incl 52
      b0.incl 64
      b0.incl 128

      let b1 = b0

      var b2 = newBitmapContainer()
      b2.incl 1
      b2.incl 10
      b2.incl 52
      b2.incl 60
      b2.incl 64
      b2.incl 128
      b2.incl 10000

      var b3 = newBitmapContainer()
      b2.incl 1
      b2.incl 10
      b2.incl 52
      b2.incl 128
      b2.incl 10000

      doAssert b0 <= b1
      doAssert not (b0 < b1)
      doAssert b0 <= b2
      doAssert b0 < b2
      doAssert not (b0 <= b3)
      doAssert not (b0 < b3)

      doAssert not (b2 <= b0)
      doAssert not (b2 < b0)
      doAssert not (b2 <= b3)
      doAssert not (b2 < b3)

      doAssert b3 <= b2
      doAssert b3 < b2

  block excl:
    ## Singular exclusion
    var bitmap = newBitmapContainer()
    bitmap.incl 10
    bitmap.incl 63
    bitmap.incl 64
    bitmap.incl 3000
    let cloned = bitmap
    var bitmapex = newBitmapContainer()
    bitmapex.incl 10
    bitmapex.incl 64
    bitmapex.incl 3000

    doAssert bitmap.len == 4

    bitmap.excl 0
    doAssert bitmap == cloned

    bitmap.excl 63
    doAssert bitmap == bitmapex

  block empty:
    ## A new bitmap is empty
    let bitmap = newBitmapContainer()
    doAssert bitmap.len == 0
    for value in 0u16..high(uint16):
      doAssert value notin bitmap:
        "Empty bitmap has value: " & $value

  block full:
    ## Bitmap can cover all of uint16
    var bitmap = newBitmapContainer()
    for value in low(uint16)..high(uint16):
      doAssert not bitmap.containsOrIncl value:
        "Empty bitmap has value: " & $value
      doAssert value in bitmap:
        "Value just added but was not in bitmap: " & $value

    doAssert bitmap.len == len(low(uint16)..high(uint16)):
      "Bitmap have less elements than the total number of uint16 values: " & $bitmap.len
    for value in low(uint16)..high(uint16):
      doAssert value in bitmap:
        "Full bitmap missing value: " & $value

  block ops:
    let empty = newBitmapContainer()

    let striped4 = block:
      var x = newBitmapContainer()
      var expectedLen = 0
      # Fill up first 100 words, on every 4th bit
      for value in countup(0u16, 64 * 100, 4):
        x.incl value
        inc expectedLen

      doAssert x.len == expectedLen:
        "Expected " & $expectedLen & " elements but got " & $x.len
      x

    let striped5 = block:
      var x = newBitmapContainer()
      var expectedLen = 0
      # Fill up 100 words, on every 5th bit
      for value in countup(64u16 * 102, 64 * (102 + 100), 5):
        x.incl value
        inc expectedLen

      doAssert x.len == expectedLen:
        "Expected " & $expectedLen & " elements but got " & $x.len
      x

    doAssert striped4.len != striped5.len:
      "These sets should have different cardinalities"

    let striped45 = block:
      var x = newBitmapContainer()
      var expectedLen = 0
      for value in countup(0u16, 64 * 100, 4):
        x.incl value
        inc expectedLen

      for value in countup(64u16 * 102, 64 * (102 + 100), 5):
        x.incl value
        inc expectedLen

      doAssert x.len == expectedLen:
        "Expected " & $expectedLen & " elements but got " & $x.len
      x

    block union:
      var b45 = striped4
      b45.incl striped5
      doAssert b45 == striped45
      doAssert striped4 + striped5 == striped45

    block intersection:
      let b4 = striped4 * striped45
      doAssert b4 == striped4
      doAssert striped4.intersectionLen(striped45) == striped4.len

      let b5 = striped5 * striped45
      doAssert b5 == striped5
      doAssert striped5.intersectionLen(striped45) == striped5.len

      let be = striped4 * striped5
      doAssert be == empty
      doAssert striped5.intersectionLen(striped4) == 0

    block difference:
      var b4 = striped45
      b4.excl striped5
      doAssert b4 == striped4
      doAssert striped45 - striped4 == striped5
      doAssert striped45.differenceLen(striped4) == striped5.len

      var b4m5 = striped4
      b4m5.excl striped5
      doAssert b4m5 == striped4
      doAssert striped4 - striped5 == striped4
      doAssert striped4.differenceLen(striped5) == striped4.len

      var be = striped5
      be.excl striped45
      doAssert be == empty
      doAssert striped5 - striped45 == empty
      doAssert striped5.differenceLen(striped45) == 0

    block symDifference:
      doAssert striped4 -+- striped5 == striped45
      doAssert striped4.symmetricDifferenceLen(striped5) == striped45.len

      doAssert striped4 -+- empty == striped4
      doAssert striped4.symmetricDifferenceLen(empty) == striped4.len

      doAssert striped4 -+- striped45 == striped5
      doAssert striped4.symmetricDifferenceLen(striped45) == striped5.len
