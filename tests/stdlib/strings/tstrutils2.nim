import "$lib/.." / compiler/utils/strutils2

block: # toLowerAscii
  var a = "fooBAr"
  a.toLowerAscii
  doAssert a == "foobar"

block: # dataPointer
  var a: string
  discard a.dataPointer
  # doAssert a.dataPointer == nil # not guaranteed
