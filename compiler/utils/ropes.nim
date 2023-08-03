#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## An implemention of ropes for use by the C and JS code generators was
## previously located here, but now ``Rope`` is only an alias for ``string``.

from pathutils import AbsoluteFile

type
  FormatStr* = string  # later we may change it to CString for better
                       # performance of the code generator (assignments
                       # copy the format strings
                       # though it is not necessary)
  Rope* = string

template rope*(s: string): Rope =
  ## A no-op -- the same as using `s` directly
  s

proc rope*(i: BiggestInt): Rope =
  ## Converts an int to a rope.
  result = $i

proc rope*(f: BiggestFloat): Rope =
  ## Converts a float to a rope.
  result = $f

proc `&`*(a: openArray[Rope]): Rope =
  ## the concatenation operator for an openarray of ropes.
  for i in 0..high(a): result = result & a[i]

proc writeRope*(f: File, r: Rope) =
  ## writes a rope to a file.
  write(f, r)

proc writeRope*(head: Rope, filename: AbsoluteFile): bool =
  var f: File
  if open(f, filename.string, fmWrite):
    writeRope(f, head)
    close(f)
    result = true
  else:
    result = false

proc ropeConcat*(a: varargs[Rope]): Rope =
  # not overloaded version of concat to speed-up `rfmt` a little bit
  for i in 0..high(a): result = result & a[i]

func prepend*(a: var Rope, b: string) = a = b & a

proc runtimeFormat*(frmt: FormatStr, args: openArray[Rope]): Rope =
  var i = 0
  result = ""
  var num = 0
  while i < frmt.len:
    if frmt[i] == '$':
      inc(i)                  # skip '$'
      case frmt[i]
      of '$':
        result.add("$")
        inc(i)
      of '#':
        inc(i)
        result.add(args[num])
        inc(num)
      of '0'..'9':
        var j = 0
        while true:
          j = j * 10 + ord(frmt[i]) - ord('0')
          inc(i)
          if i >= frmt.len or frmt[i] notin {'0'..'9'}: break
        num = j
        if j > high(args) + 1:
          doAssert false, "invalid format string: " & frmt
        else:
          result.add(args[j-1])
      of '{':
        inc(i)
        var j = 0
        while frmt[i] in {'0'..'9'}:
          j = j * 10 + ord(frmt[i]) - ord('0')
          inc(i)
        num = j
        if frmt[i] == '}': inc(i)
        else:
          doAssert false, "invalid format string: " & frmt

        if j > high(args) + 1:
          doAssert false, "invalid format string: " & frmt
        else:
          result.add(args[j-1])
      of 'n':
        result.add("\n")
        inc(i)
      of 'N':
        result.add("\n")
        inc(i)
      else:
        doAssert false, "invalid format string: " & frmt
    var start = i
    while i < frmt.len:
      if frmt[i] != '$': inc(i)
      else: break
    if i - 1 >= start:
      result.add(substr(frmt, start, i - 1))

proc `%`*(frmt: static[FormatStr], args: openArray[Rope]): Rope =
  runtimeFormat(frmt, args)

template addf*(c: var Rope, frmt: FormatStr, args: openArray[Rope]) =
  ## shortcut for ``add(c, frmt % args)``.
  c.add(frmt % args)

when true:
  template `~`*(r: string): Rope = r % []

const
  bufSize = 1024              # 1 KB is reasonable

proc equalsFile*(s: Rope, f: File): bool =
  ## returns true if the contents of the file `f` equal `r`.
  var
    buf: array[bufSize, char]
    bpos = buf.len
    blen = buf.len
    btotal = 0
    rtotal = 0

  when true:
    var spos = 0
    rtotal += s.len
    while spos < s.len:
      if bpos == blen:
        # Read more data
        bpos = 0
        blen = readBuffer(f, addr(buf[0]), buf.len)
        btotal += blen
        if blen == 0:  # no more data in file
          result = false
          return
      let n = min(blen - bpos, s.len - spos)
      # TODO There's gotta be a better way of comparing here...
      if not equalMem(addr(buf[bpos]), cast[pointer](cast[int](cstring(s))+spos), n):
        result = false
        return
      spos += n
      bpos += n

  result = readBuffer(f, addr(buf[0]), 1) == 0 and
      btotal == rtotal # check that we've read all

proc equalsFile*(r: Rope, filename: AbsoluteFile): bool =
  ## returns true if the contents of the file `f` equal `r`. If `f` does not
  ## exist, false is returned.
  var f: File
  result = open(f, filename.string)
  if result:
    result = equalsFile(r, f)
    close(f)

proc writeRopeIfNotEqual*(r: Rope, filename: AbsoluteFile): bool =
  # returns true if overwritten
  if not equalsFile(r, filename):
    result = writeRope(r, filename)
  else:
    result = false
