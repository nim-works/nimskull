discard """
  description: "Test packedsets array container"
"""

import std/private/packedsets/arrays
import std/sequtils

block basic:
  ## Basic property tests
  block unsorted:
    ## Unsorted insertion works
    const
      values = [40000u16, 16, 300, 20, 16, 640, 300]
      sortedValues = [16u16, 20, 300, 640, 40000]

    var array: ArrayContainer
    for value in values:
      array.incl value

    var asSeq = array.toSeq()
    doAssert asSeq == sortedValues:
      "Unexpected values: " & $asSeq

  block full:
    ## Array container can cover all of uint16
    var array: ArrayContainer
    for value in low(uint16)..high(uint16):
      doAssert not array.containsOrIncl value:
        "Empty array has value: " & $value
      doAssert value in array:
        "Value just added but was not in array: " & $value

    doAssert array.len == len(low(uint16)..high(uint16)):
      "Array have less elements than the total number of uint16 values: " & $array.len
    for value in low(uint16)..high(uint16):
      doAssert value in array:
        "Full array missing value: " & $value

  block comparison:
    block subset:
      ## Verify subset test operators
      var b0: ArrayContainer
      b0.incl 1
      b0.incl 52
      b0.incl 64
      b0.incl 128

      let b1 = b0

      var b2: ArrayContainer
      b2.incl 1
      b2.incl 10
      b2.incl 52
      b2.incl 60
      b2.incl 64
      b2.incl 128
      b2.incl 10000

      var b3: ArrayContainer
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
    var array: ArrayContainer
    array.incl 10
    array.incl 63
    array.incl 64
    array.incl 3000
    let cloned = array
    var arrayex: ArrayContainer
    arrayex.incl 10
    arrayex.incl 64
    arrayex.incl 3000

    doAssert array.len == 4

    array.excl 0
    doAssert array == cloned

    array.excl 63
    doAssert array == arrayex

  block ops:
    let empty = newArrayContainer()

    let striped4 = block:
      var x: ArrayContainer
      var expectedLen = 0
      for value in countup(0u16, 64 * 100, 4):
        x.incl value
        inc expectedLen

      doAssert x.len == expectedLen:
        "Expected " & $expectedLen & " elements but got " & $x.len
      x

    let mini4 = block:
      var x: ArrayContainer
      var expectedLen = 0
      for value in countup(0u16, 64, 4):
        x.incl value
        inc expectedLen

      doAssert x.len == expectedLen:
        "Expected " & $expectedLen & " elements but got " & $x.len
      x

    let striped5 = block:
      var x: ArrayContainer
      var expectedLen = 0
      for value in countup(64u16 * 102, 64 * (102 + 100), 5):
        x.incl value
        inc expectedLen

      doAssert x.len == expectedLen:
        "Expected " & $expectedLen & " elements but got " & $x.len
      x

    doAssert mini4.len * 64 < striped4.len:
      "mini4 should be smaller than striped4 by a factor of 64: " & $(mini4: mini4.len, striped4: striped4.len)
    doAssert striped4.len != striped5.len:
      "These sets should have different cardinalities"

    let striped45 = block:
      var x: ArrayContainer
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

      var b4 = striped4
      b4.incl mini4
      doAssert b4 == striped4
      doAssert mini4 + striped4 == striped4

    block intersection:
      let b4 = striped4 * striped45
      doAssert b4 == striped4

      let b5 = striped5 * striped45
      doAssert b5 == striped5

      # This tests the galloping intersection algorithm
      let bm4 = mini4 * striped45
      doAssert bm4 == mini4

      let be = striped4 * striped5
      doAssert be == empty

    block difference:
      var b4 = striped45
      b4.excl striped5
      doAssert b4 == striped4
      doAssert striped45 - striped4 == striped5

      var b4m5 = striped4
      b4m5.excl striped5
      doAssert b4m5 == striped4
      doAssert striped4 - striped5 == striped4

      var be = striped5
      be.excl striped45
      doAssert be == empty
      doAssert striped5 - striped45 == empty

    block symDifference:
      doAssert striped4 -+- striped5 == striped45

      doAssert striped4 -+- empty == striped4

      doAssert striped4 -+- striped45 == striped5
