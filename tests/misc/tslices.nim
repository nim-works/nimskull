discard """
output: '''
456456
456456
456456
Zugr5nd
egerichtetd
verichtetd
'''
"""

import experimental/property_testing

# Test the new slices.

var mystr = "Abgrund"
# mystr[..1] = "Zu" # deprecated
mystr[0..1] = "Zu"

mystr[4..4] = "5"

type
  TEnum = enum e1, e2, e3, e4, e5, e6

var myarr: array[TEnum, int] = [1, 2, 3, 4, 5, 6]
myarr[e1..e3] = myarr[e4..e6]
# myarr[..e3] = myarr[e4..e6] # deprecated
myarr[0..e3] = myarr[e4..e6]

for x in items(myarr): stdout.write(x)
echo()

var myarr2: array[0..5, int] = [1, 2, 3, 4, 5, 6]
myarr2[0..2] = myarr2[3..5]

for x in items(myarr2): stdout.write(x)
echo()


var myseq = @[1, 2, 3, 4, 5, 6]
myseq[0..2] = myseq[^3 .. ^1]

for x in items(myseq): stdout.write(x)
echo()

echo mystr

mystr[4..4] = "u"

# test full replacement
# mystr[.. ^2] = "egerichtet"  # deprecated
mystr[0 .. ^2] = "egerichtet"

echo mystr

mystr[0..2] = "ve"
echo mystr

var s = "abcdef"
s[1 .. ^2] = "xyz"
assert s == "axyzf"

proc genArrayWithIdx[Idx, T](g: sink Gen[T], _: typedesc[Idx]): Gen[array[Idx, T]] =
  const size = uint32 len(default(array[Idx, bool]))
  genArray(g, size).map(
    proc (x: array[size, T]): array[Idx, T] = x
  )

proc genSlice[T](first: T, last: T): Gen[Slice[T]] =
  let genOrd = genInt(ord(first), ord(last)).map(
    proc (x: int): T = T(x)
  )
  genArray(genOrd, 2).map(
    proc (x: array[2, T]): Slice[T] =
      x[0]..x[1]
  )

proc slicingProperty[T](genList: Gen[T]) =
  let genListAndSlice = genList.flatMap(
    proc (x: T): auto =
      genTuple: (
        list: genConst(x),
        slice: if len(x) == 0:
                 genConst(low(x)..high(x))
               else:
                 genSlice(low(x), high(x))
      )
  )

  block:
    ## Sliced list must contains selected items in their original ordering
    let prop = forAll (input: genListAndSlice):
      let
        (list, slice) = input
        testList = list[slice]

      if len(testList) != len(slice):
        return psFail

      var tidx = 0
      for idx in slice:
        if list[idx] != testList[tidx]:
          return psFail

        inc tidx

      return psPass

    let result = runProperty(prop)
    assert result.status == psPass, $result

slicingProperty(genSeq(genInt()))
slicingProperty(genString())
slicingProperty(genArrayWithIdx(genInt(), range[-10..100]))
slicingProperty(genArrayWithIdx(genInt(), range[0u32..100u32]))
slicingProperty(genArrayWithIdx(genInt(), TEnum))
