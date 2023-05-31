discard """
"""

import std/[unittest, parseopt2, sequtils]

template checkOpts(args; expected) =
  check(toSeq(args) == expected)

## test edge cases

func allRequired[T](a: openArray[T]): seq[(T, OptValExpectation)] =
  for x in a:
    result &= (x, optValRequired)

checkOpts opts(["-c", "c_val"], optValNone, shortVal=['c'].allRequired),
  @[Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-c:c_val"], optValNone, shortVal=['c'].allRequired),
  @[Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-c=c_val"], optValNone, shortVal=['c'].allRequired),
  @[Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-cc_val", "-j4"], optValNone, shortVal=['c', 'j'].allRequired),
  @[Opt(kind: optShort, keyShort: 'c', val: "c_val"), Opt(kind: optShort, keyShort: 'j', val: "4")]
checkOpts opts(["-abcc_val"], optValNone, shortVal=['c'].allRequired),
  @[Opt(kind: optShort, keyShort: 'a', val: ""), Opt(kind: optShort, keyShort: 'b', val: ""), Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-abc", "c_val"], optValNone, shortVal=['c'].allRequired),
  @[Opt(kind: optShort, keyShort: 'a', val: ""), Opt(kind: optShort, keyShort: 'b', val: ""), Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-abc:c_val"], optValNone, shortVal=['c'].allRequired),
  @[Opt(kind: optShort, keyShort: 'a', val: ""), Opt(kind: optShort, keyShort: 'b', val: ""), Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-abc=c_val"], optValNone, shortVal=['c'].allRequired),
  @[Opt(kind: optShort, keyShort: 'a', val: ""), Opt(kind: optShort, keyShort: 'b', val: ""), Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["--long", "val"], optValNone, longVal=["long"].allRequired),
  @[Opt(kind: optLong, keyLong: "long", val: "val")]
checkOpts opts(["--long:val"], optValNone, longVal=["long"].allRequired),
  @[Opt(kind: optLong, keyLong: "long", val: "val")]
checkOpts opts(["--long=val"], optValNone, longVal=["long"].allRequired),
  @[Opt(kind: optLong, keyLong: "long", val: "val")]
checkOpts opts(["--long="], optValNone, longVal=["long"].allRequired),
  @[Opt(kind: optLong, keyLong: "long", val: "")]
checkOpts opts(["--", "-positional1", "--positional2", "positional3"], optValNone),
  @[Opt(kind: optPos, val: "-positional1"), Opt(kind: optPos, val: "--positional2"), Opt(kind: optPos, val: "positional3")]
checkOpts opts(["-"], optValNone),
  @[Opt(kind: optPos, val: "-")]
let emptySeq: seq[Opt] = @[]
checkOpts opts(["--"], optValNone), emptySeq
checkOpts opts(["---"], optValNone),
  @[Opt(kind: optLong, keyLong: "-", val: "")]


## mixed test
import std/strutils
checkOpts opts("hello world! -abcde --f:g --h=i --j --k l m".split(" "), optValNone, shortVal=['c'].allRequired, longVal=["f","h","k"].allRequired), @[
  Opt(kind: optPos, val: "hello"),
  Opt(kind: optPos, val: "world!"),
  Opt(kind: optShort, keyShort: 'a', val: ""),
  Opt(kind: optShort, keyShort: 'b', val: ""),
  Opt(kind: optShort, keyShort: 'c', val: "de"),
  Opt(kind: optLong, keyLong: "f", val: "g"),
  Opt(kind: optLong, keyLong: "h", val: "i"),
  Opt(kind: optLong, keyLong: "j", val: ""),
  Opt(kind: optLong, keyLong: "k", val: "l"),
  Opt(kind: optPos, val: "m"),
]

# todo: test exceptions