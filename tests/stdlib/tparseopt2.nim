discard """
"""

import std/[unittest, parseopt2, sequtils]

template checkOpts(args; expected) =
  check(toSeq(args) == expected)

# test edge cases

func allRequired[T](a: openArray[T]): seq[(T, OptValExpectation)] =
  for x in a:
    result &= (x, optValRequired)

checkOpts opts(["-c", "c_val"], shortVal=['c'].allRequired),
  @[Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-c:c_val"], shortVal=['c'].allRequired),
  @[Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-c=c_val"], shortVal=['c'].allRequired),
  @[Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-cc_val", "-j4"], shortVal=['c', 'j'].allRequired),
  @[Opt(kind: optShort, keyShort: 'c', val: "c_val"), Opt(kind: optShort, keyShort: 'j', val: "4")]
checkOpts opts(["-abcc_val"], shortVal=['c'].allRequired),
  @[Opt(kind: optShort, keyShort: 'a', val: ""), Opt(kind: optShort, keyShort: 'b', val: ""), Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-abc", "c_val"], shortVal=['c'].allRequired),
  @[Opt(kind: optShort, keyShort: 'a', val: ""), Opt(kind: optShort, keyShort: 'b', val: ""), Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-abc:c_val"], shortVal=['c'].allRequired),
  @[Opt(kind: optShort, keyShort: 'a', val: ""), Opt(kind: optShort, keyShort: 'b', val: ""), Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-abc=c_val"], shortVal=['c'].allRequired),
  @[Opt(kind: optShort, keyShort: 'a', val: ""), Opt(kind: optShort, keyShort: 'b', val: ""), Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["--long", "val"], longVal=["long"].allRequired),
  @[Opt(kind: optLong, keyLong: "long", val: "val")]
checkOpts opts(["--long:val"], longVal=["long"].allRequired),
  @[Opt(kind: optLong, keyLong: "long", val: "val")]
checkOpts opts(["--long=val"], longVal=["long"].allRequired),
  @[Opt(kind: optLong, keyLong: "long", val: "val")]
checkOpts opts(["--long="], longVal=["long"].allRequired),
  @[Opt(kind: optLong, keyLong: "long", val: "")]
checkOpts opts(["--", "-positional1", "--positional2", "positional3"]),
  @[Opt(kind: optPos, val: "-positional1"), Opt(kind: optPos, val: "--positional2"), Opt(kind: optPos, val: "positional3")]
checkOpts opts(["-"]),
  @[Opt(kind: optPos, val: "-")]
let emptySeq: seq[Opt] = @[]
checkOpts opts(["--"]), emptySeq
checkOpts opts(["---"]),
  @[Opt(kind: optLong, keyLong: "-", val: "")]


# mixed tests

import std/strutils
checkOpts opts("hello world! -abcde --f:g --h=i --j --k l m".split(" "), shortVal=['c'].allRequired, longVal=["f","h","k"].allRequired),
  @[
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

# optional tests
# the way to make short opt simple flag, but long opt optionally on/off

checkOpts opts(["--foo"]),
  @[Opt(kind: optLong, keyLong: "foo", val: "")]
checkOpts opts(["--foo:bar"]),
  @[Opt(kind: optLong, keyLong: "foo", val: "bar")]
checkOpts opts(["--foo=bar"]),
  @[Opt(kind: optLong, keyLong: "foo", val: "bar")]
checkOpts opts(["-fbar"]),
  @[
    Opt(kind: optShort, keyShort: 'f', val: ""),
    Opt(kind: optShort, keyShort: 'b', val: ""),
    Opt(kind: optShort, keyShort: 'a', val: ""),
    Opt(kind: optShort, keyShort: 'r', val: ""),
  ]

# todo: test exceptions
