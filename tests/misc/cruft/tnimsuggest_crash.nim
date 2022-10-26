discard """
  cmd: "nim check --hints:off --warnings:off $file"
  errormsg: "undeclared identifier: 'Undeclared'"
  target: native
  description: '''
    . From https://github.com/nim-lang/Nim/issues/12684
      nimsuggest crash with import after encountering undeclared type
  '''

"""

var x: Undeclared
import std/strutils
