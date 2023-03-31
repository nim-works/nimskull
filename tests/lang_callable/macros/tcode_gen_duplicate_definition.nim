discard """
  action: "compile"
  target: "!vm"
  description: '''
  . Originally from https://github.com/nim-lang/Nim/issues/6986 as a duplicate
    cpp codegen issue, but this is more testing the invariant for all backends
    and the macros library
  '''
"""

# broken on VM, requires deeper dive (knownIssue)

import std/[sequtils, strutils]


let rules = toSeq(lines("input"))
  .mapIt(it.split(" => ").mapIt(it.replace("/", "")))
  .mapIt((it[0], it[1]))


proc pp(s: string): auto =
  toSeq(lines(s)).mapIt(it.split(" => ").mapIt(it.replace("/", ""))).mapIt((it[0], it[1]))
echo pp("input")