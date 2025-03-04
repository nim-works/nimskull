discard """
  description: "Make sure some code can be compiled with time-tracing enabled"
  targets: "c js vm"
  matrix: "--timetrace:on"
"""

# import some modules so that there's some processing for the compiler
# to trace
import std/[strutils, macros]
