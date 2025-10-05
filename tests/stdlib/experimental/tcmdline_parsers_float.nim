discard """
  description: "Tests for the cmdline float parser"
  targets: "c js vm"
"""
import std/math
import experimental/cmdline/parsers
# TODO: Merge into tcmdline_parsers.nim once `math.isNaN` is implemented for VM

block floatParser:
  doAssert parseCli(float64, "3.14") == 3.14
  doAssert parseCli(float64, "-3.14") == -3.14
  doAssert parseCli(float32, "nan").isNaN
  doAssert parseCli(float32, "inf") == Inf
  doAssert parseCli(float32, "-inf") == -Inf