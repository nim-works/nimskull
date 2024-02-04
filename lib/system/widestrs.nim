#
#
#            Nim's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# Nim support for C's `wide strings`:idx:. This is part of the system
# module! Do not import it directly!

#when not declared(ThisIsSystem):
#  {.error: "You must not import this module explicitly".}

type
  Utf16Char* = distinct int16

when true:
  type
    WideCString* = ptr UncheckedArray[Utf16Char]

    WideCStringObj* = object
      data: seq[Utf16Char] ## the raw character buffer. The number of elements
                           ## doesn't necessarily correspond to the length of
                           ## the string

  proc `=copy`(a: var WideCStringObj; b: WideCStringObj) {.error.}

  proc createWide(a: var WideCStringObj; len: int) =
    a.data.newSeq(len)

  template `[]`*(a: WideCStringObj; idx: int): Utf16Char = a.data[idx]
  template `[]=`*(a: WideCStringObj; idx: int; val: Utf16Char) = a.data[idx] = val

  template nullWide(): untyped = WideCStringObj()

  converter toWideCString*(x: WideCStringObj): WideCString {.inline.} =
    if x.data.len > 0:
      result = cast[WideCString](unsafeAddr x.data[0])
    else:
      result = nil

proc ord(arg: Utf16Char): int = int(cast[uint16](arg))

proc len*(w: WideCString): int =
  ## returns the length of a widestring. This traverses the whole string to
  ## find the binary zero end marker!
  result = 0
  while int16(w[result]) != 0'i16: inc result

const
  UNI_REPLACEMENT_CHAR = Utf16Char(0xFFFD'i16)
  UNI_MAX_BMP = 0x0000FFFF
  UNI_MAX_UTF16 = 0x0010FFFF
  # UNI_MAX_UTF32 = 0x7FFFFFFF
  # UNI_MAX_LEGAL_UTF32 = 0x0010FFFF

  halfShift = 10
  halfBase = 0x0010000
  halfMask = 0x3FF

  UNI_SUR_HIGH_START = 0xD800
  UNI_SUR_HIGH_END = 0xDBFF
  UNI_SUR_LOW_START = 0xDC00
  UNI_SUR_LOW_END = 0xDFFF
  UNI_REPL = 0xFFFD

template ones(n: untyped): untyped = ((1 shl n)-1)

template fastRuneAt(s: cstring, i, L: int, result: untyped, doInc = true) =
  ## Returns the unicode character `s[i]` in `result`. If `doInc == true`
  ## `i` is incremented by the number of bytes that have been processed.
  bind ones

  if ord(s[i]) <= 127:
    result = ord(s[i])
    when doInc: inc(i)
  elif ord(s[i]) shr 5 == 0b110:
    #assert(ord(s[i+1]) shr 6 == 0b10)
    if i <= L - 2:
      result = (ord(s[i]) and (ones(5))) shl 6 or (ord(s[i+1]) and ones(6))
      when doInc: inc(i, 2)
    else:
      result = UNI_REPL
      when doInc: inc(i)
  elif ord(s[i]) shr 4 == 0b1110:
    if i <= L - 3:
      #assert(ord(s[i+1]) shr 6 == 0b10)
      #assert(ord(s[i+2]) shr 6 == 0b10)
      result = (ord(s[i]) and ones(4)) shl 12 or
               (ord(s[i+1]) and ones(6)) shl 6 or
               (ord(s[i+2]) and ones(6))
      when doInc: inc(i, 3)
    else:
      result = UNI_REPL
      when doInc: inc(i)
  elif ord(s[i]) shr 3 == 0b11110:
    if i <= L - 4:
      #assert(ord(s[i+1]) shr 6 == 0b10)
      #assert(ord(s[i+2]) shr 6 == 0b10)
      #assert(ord(s[i+3]) shr 6 == 0b10)
      result = (ord(s[i]) and ones(3)) shl 18 or
               (ord(s[i+1]) and ones(6)) shl 12 or
               (ord(s[i+2]) and ones(6)) shl 6 or
               (ord(s[i+3]) and ones(6))
      when doInc: inc(i, 4)
    else:
      result = UNI_REPL
      when doInc: inc(i)
  else:
    result = 0xFFFD
    when doInc: inc(i)

iterator runes(s: cstring, L: int): int =
  var
    i = 0
    result: int
  while i < L:
    fastRuneAt(s, i, L, result, true)
    yield result

proc newWideCString*(size: int): WideCStringObj =
  ## Create an empty `WideCStringObj` with the given buffer size in
  ## UTF-16 code units.
  createWide(result, size + 1) # +1 for the zero terminator

proc newWideCString*(source: cstring, L: int): WideCStringObj =
  ## Take a `cstring` and its length `L` in bytes, then returns a copy in wide form.
  createWide(result, L + 1) # +1 for the zero terminator
  var d = 0
  for ch in runes(source, L):

    if ch <= UNI_MAX_BMP:
      if ch >= UNI_SUR_HIGH_START and ch <= UNI_SUR_LOW_END:
        result[d] = UNI_REPLACEMENT_CHAR
      else:
        result[d] = cast[Utf16Char](uint16(ch))
    elif ch > UNI_MAX_UTF16:
      result[d] = UNI_REPLACEMENT_CHAR
    else:
      let ch = ch - halfBase
      result[d] = cast[Utf16Char](uint16((ch shr halfShift) + UNI_SUR_HIGH_START))
      inc d
      result[d] = cast[Utf16Char](uint16((ch and halfMask) + UNI_SUR_LOW_START))
    inc d
  result[d] = Utf16Char(0)

proc newWideCString*(s: cstring): WideCStringObj =
  ## Returns a copy of the string `s` in wide form. `s` should be NUL-terminated.
  if s.isNil: return nullWide

  result = newWideCString(s, s.len)

proc newWideCString*(s: string): WideCStringObj =
  ## Returns a copy of the string `s` in wide form.
  result = newWideCString(cstring s, s.len)

proc `$`*(w: WideCString, estimate: int, replacement: int = 0xFFFD): string =
  result = newStringOfCap(estimate + estimate shr 2)

  var i = 0
  while w[i].int16 != 0'i16:
    var ch = ord(w[i])
    inc i
    if ch >= UNI_SUR_HIGH_START and ch <= UNI_SUR_HIGH_END:
      # If the 16 bits following the high surrogate are in the source buffer...
      let ch2 = ord(w[i])

      # If it's a low surrogate, convert to UTF32:
      if ch2 >= UNI_SUR_LOW_START and ch2 <= UNI_SUR_LOW_END:
        ch = (((ch and halfMask) shl halfShift) + (ch2 and halfMask)) + halfBase
        inc i
      else:
        #invalid UTF-16
        ch = replacement
    elif ch >= UNI_SUR_LOW_START and ch <= UNI_SUR_LOW_END:
      #invalid UTF-16
      ch = replacement

    if ch < 0x80:
      result.add chr(ch)
    elif ch < 0x800:
      result.add chr((ch shr 6) or 0xc0)
      result.add chr((ch and 0x3f) or 0x80)
    elif ch < 0x10000:
      result.add chr((ch shr 12) or 0xe0)
      result.add chr(((ch shr 6) and 0x3f) or 0x80)
      result.add chr((ch and 0x3f) or 0x80)
    elif ch <= 0x10FFFF:
      result.add chr((ch shr 18) or 0xf0)
      result.add chr(((ch shr 12) and 0x3f) or 0x80)
      result.add chr(((ch shr 6) and 0x3f) or 0x80)
      result.add chr((ch and 0x3f) or 0x80)
    else:
      # replacement char(in case user give very large number):
      result.add chr(0xFFFD shr 12 or 0b1110_0000)
      result.add chr(0xFFFD shr 6 and ones(6) or 0b10_0000_00)
      result.add chr(0xFFFD and ones(6) or 0b10_0000_00)

proc `$`*(s: WideCString): string =
  result = s $ 80

when true:
  proc `$`*(s: WideCStringObj, estimate: int, replacement: int = 0xFFFD): string =
    `$`(toWideCString(s), estimate, replacement)

  proc `$`*(s: WideCStringObj): string =
    $(toWideCString(s))
