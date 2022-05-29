discard """
  cmd: "nim check --hints:off --warnings:off $file"
  errormsg: "undeclared identifier: 'Undeclared'"
  target: native
"""

var x: Undeclared
import std/strutils
