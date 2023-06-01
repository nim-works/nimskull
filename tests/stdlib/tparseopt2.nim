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
  @[Opt(kind: short, keyShort: 'c', value: "c_val")]
checkOpts opts(["-c:c_val"], shortVal=['c'].allRequired),
  @[Opt(kind: short, keyShort: 'c', value: "c_val")]
checkOpts opts(["-c=c_val"], shortVal=['c'].allRequired),
  @[Opt(kind: short, keyShort: 'c', value: "c_val")]
checkOpts opts(["-cc_val", "-j4"], shortVal=['c', 'j'].allRequired),
  @[Opt(kind: short, keyShort: 'c', value: "c_val"), Opt(kind: short, keyShort: 'j', value: "4")]
checkOpts opts(["-abcc_val"], shortVal=['c'].allRequired),
  @[Opt(kind: short, keyShort: 'a', value: ""), Opt(kind: short, keyShort: 'b', value: ""), Opt(kind: short, keyShort: 'c', value: "c_val")]
checkOpts opts(["-abc", "c_val"], shortVal=['c'].allRequired),
  @[Opt(kind: short, keyShort: 'a', value: ""), Opt(kind: short, keyShort: 'b', value: ""), Opt(kind: short, keyShort: 'c', value: "c_val")]
checkOpts opts(["-abc:c_val"], shortVal=['c'].allRequired),
  @[Opt(kind: short, keyShort: 'a', value: ""), Opt(kind: short, keyShort: 'b', value: ""), Opt(kind: short, keyShort: 'c', value: "c_val")]
checkOpts opts(["-abc=c_val"], shortVal=['c'].allRequired),
  @[Opt(kind: short, keyShort: 'a', value: ""), Opt(kind: short, keyShort: 'b', value: ""), Opt(kind: short, keyShort: 'c', value: "c_val")]
checkOpts opts(["--long", "val"], longVal=["long"].allRequired),
  @[Opt(kind: long, keyLong: "long", value: "val")]
checkOpts opts(["--long:val"], longVal=["long"].allRequired),
  @[Opt(kind: long, keyLong: "long", value: "val")]
checkOpts opts(["--long=val"], longVal=["long"].allRequired),
  @[Opt(kind: long, keyLong: "long", value: "val")]
checkOpts opts(["--long="], longVal=["long"].allRequired),
  @[Opt(kind: long, keyLong: "long", value: "")]
checkOpts opts(["--", "-positional1", "--positional2", "positional3"]),
  @[Opt(kind: positional, value: "-positional1"), Opt(kind: positional, value: "--positional2"), Opt(kind: positional, value: "positional3")]
checkOpts opts(["-"]),
  @[Opt(kind: positional, value: "-")]
let emptySeq: seq[Opt] = @[]
checkOpts opts(["--"]), emptySeq
checkOpts opts(["---"]),
  @[Opt(kind: long, keyLong: "-", value: "")]


# mixed tests

import std/strutils
checkOpts opts("hello world! -abcde --f:g --h=i --j --k l m".split(" "), shortVal=['c'].allRequired, longVal=["f","h","k"].allRequired),
  @[
    Opt(kind: positional, value: "hello"),
    Opt(kind: positional, value: "world!"),
    Opt(kind: short, keyShort: 'a', value: ""),
    Opt(kind: short, keyShort: 'b', value: ""),
    Opt(kind: short, keyShort: 'c', value: "de"),
    Opt(kind: long, keyLong: "f", value: "g"),
    Opt(kind: long, keyLong: "h", value: "i"),
    Opt(kind: long, keyLong: "j", value: ""),
    Opt(kind: long, keyLong: "k", value: "l"),
    Opt(kind: positional, value: "m"),
  ]

# optional tests
# the way to make short opt simple flag, but long opt optionally on/off

checkOpts opts(["--foo"]),
  @[Opt(kind: long, keyLong: "foo", value: "")]
checkOpts opts(["--foo:bar"]),
  @[Opt(kind: long, keyLong: "foo", value: "bar")]
checkOpts opts(["--foo=bar"]),
  @[Opt(kind: long, keyLong: "foo", value: "bar")]
checkOpts opts(["-fbar"]),
  @[
    Opt(kind: short, keyShort: 'f', value: ""),
    Opt(kind: short, keyShort: 'b', value: ""),
    Opt(kind: short, keyShort: 'a', value: ""),
    Opt(kind: short, keyShort: 'r', value: ""),
  ]

# todo: test exceptions
