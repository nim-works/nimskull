discard """
  targets: "!js"
  action: compile
"""

import terminal, colors

styledEcho fgColor, colRed, "Test"
styledEcho bgColor, colBlue, "Test"
