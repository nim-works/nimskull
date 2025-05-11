discard """
  targets: "c js vm"
  knownIssue.vm: '''
    toOpenArrayByte returns an openArray[char] internally, causing a run-time
    type error
  '''
"""

proc toSeqByte(a: openArray[byte]): seq[byte] {.noinline.} =
  for x in a.items:
    result.add x

let str = "0123456789"
doAssert toSeqByte(toOpenArrayByte(str, 0, str.high)) ==
         [0x30'u8, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39]
