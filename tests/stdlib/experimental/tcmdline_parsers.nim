discard """
  description: "Tests for the cmdline parsers module"
  targets: "c js vm"
"""

import std/math
import experimental/cmdline/parsers

block intParser:
  doAssert parseCli(int, "99") == 99
  doAssert parseCli(int, "-99") == -99
  doAssert parseCli(uint8, "255") == 255
  doAssertRaises(ValueError):
    discard parseCli(int8, "255")
  doAssertRaises(ValueError):
    discard parseCli(uint8, "256")
  doAssertRaises(ValueError):
    discard parseCli(uint8, "-1")

block floatParser:
  doAssert parseCli(float64, "3.14") == 3.14
  doAssert parseCli(float64, "-3.14") == -3.14
  doAssert parseCli(float32, "nan").isNaN
  doAssert parseCli(float32, "inf") == Inf
  doAssert parseCli(float32, "-inf") == -Inf

block stringParser:
  doAssert parseCli(string, "foo bar") == "foo bar"

block boolParser:
  doAssert parseCli(bool, "true") == true
  doAssert parseCli(bool, "false") == false
  doAssertRaises(ValueError):
    discard parseCli(bool, "fool")

block enumParser:
  type
    Colour = enum
      Red, Green

  doAssert parseCli(Colour, "Red") == Red
  doAssert parseCli(Colour, "Green") == Green
  doAssertRaises(ValueError):
    discard parseCli(Colour, "Blue")

block rangeParser:
  doAssert parseCli(range[0..99], "99") == 99
  doAssert parseCli(range[0..99], "0") == 0
  doAssert parseCli(range[0..99], "50") == 50
  doAssertRaises(ValueError):
    discard parseCli(range[0..99], "100")