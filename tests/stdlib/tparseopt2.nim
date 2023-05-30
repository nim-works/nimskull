discard """
"""

import std/[unittest, parseopt2, sequtils]

template checkopts(args; expected) =
  check(toSeq(args) == expected)

## test edge cases

checkOpts opts(["-c", "c_val"], shortHasVal="c"),
  @[Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-c:c_val"], shortHasVal="c"),
  @[Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-c=c_val"], shortHasVal="c"),
  @[Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-abcc_val"], shortHasVal="c"),
  @[Opt(kind: optShort, keyShort: 'a', val: ""), Opt(kind: optShort, keyShort: 'b', val: ""), Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-abc", "c_val"], shortHasVal="c"),
  @[Opt(kind: optShort, keyShort: 'a', val: ""), Opt(kind: optShort, keyShort: 'b', val: ""), Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-abc:c_val"], shortHasVal="c"),
  @[Opt(kind: optShort, keyShort: 'a', val: ""), Opt(kind: optShort, keyShort: 'b', val: ""), Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["-abc=c_val"], shortHasVal="c"),
  @[Opt(kind: optShort, keyShort: 'a', val: ""), Opt(kind: optShort, keyShort: 'b', val: ""), Opt(kind: optShort, keyShort: 'c', val: "c_val")]
checkOpts opts(["--long", "val"], longHasVal=["long"]),
  @[Opt(kind: optLong, keyLong: "long", val: "val")]
checkOpts opts(["--long:val"], longHasVal=["long"]),
  @[Opt(kind: optLong, keyLong: "long", val: "val")]
checkOpts opts(["--long=val"], longHasVal=["long"]),
  @[Opt(kind: optLong, keyLong: "long", val: "val")]
checkOpts opts(["--long="], longHasVal=["long"]),
  @[Opt(kind: optLong, keyLong: "long", val: "")]
checkOpts opts(["--", "-positional1", "--positional2", "positional3"]),
  @[Opt(kind: optPos, val: "-positional1"), Opt(kind: optPos, val: "--positional2"), Opt(kind: optPos, val: "positional3")]
checkOpts opts(["-"]),
  @[Opt(kind: optPos, val: "-")]
let emptySeq: seq[Opt] = @[]
checkOpts opts(["--"]), emptySeq
checkOpts opts(["---"]),
  @[Opt(kind: optLong, keyLong: "-", val: "")]


## mixed test
import std/strutils
checkOpts opts("hello world! -abcde --f:g --h=i --j --k l m".split(" "), shortHasVal="c", longHasVal=["f","h","k"]), @[
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