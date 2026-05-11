discard """
  action: compile
"""

import std/[terminal, colors]

styledEcho fgColor, colRed, "Test"
styledEcho bgColor, colBlue, "Test"
