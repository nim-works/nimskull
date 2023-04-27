discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/7097
    Weird interaction between converters and templates
  . Results in a strange error message "Error:
    'result.data.bytes.bytes[result.data.bytes.ibegin + result.data.ibegin + i]'
    cannot be assigned to"
  . The problem goes away if the converter definition is removed or
    if the []= template is changed to a regular proc.
  . Works if you make BytesRange a ref object.
'''
"""

type
  Byte* = uint8
  Bytes* = seq[Byte]

  BytesRange* = object
    bytes: Bytes
    ibegin, iend: int

proc initBytesRange*(s: var Bytes, ibegin = 0, iend = -1): BytesRange =
  let e = if iend < 0: s.len + iend + 1
          else: iend
  assert ibegin > 0 and e <= s.len

  result.bytes = s
  result.ibegin = ibegin
  result.iend = e

template `[]=`*(r: var BytesRange, i: int, v: Byte) =
  r.bytes[r.ibegin + i] = v

converter fromSeq*(s: Bytes): BytesRange =
  var seqCopy = s
  return initBytesRange(seqCopy)

type
 Reader* = object
    data: BytesRange
    position: int

proc readerFromHex*(input: string): Reader =
  let totalBytes = input.len div 2
  var backingStore = newSeq[Byte](totalBytes)
  result.data = initBytesRange(backingStore)

  for i in 0 ..< totalBytes:
    var nextByte = 0
    result.data[i] = Byte(nextByte) # <-------- instantiated from here