
# bug #5237

import std/tables
import std/sets
import std/sequtils


const EXTENSIONMAP = {
  "c": @["*.c", "*.h"],
}.toTable()

const EXTENSIONS = toHashSet(concat(toSeq(EXTENSIONMAP.values())))
