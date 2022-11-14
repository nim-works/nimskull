discard """
  targets: "c js"
"""

import std/strtabs

macro m =
  var t = {"name": "John"}.newStringTable
  doAssert t["name"] == "John"

block:
  var t = {"name": "John"}.newStringTable
  doAssert t["name"] == "John"

m()
