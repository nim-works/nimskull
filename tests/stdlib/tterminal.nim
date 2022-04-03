discard """
  targets: "c cpp"
  action: compile
"""

import terminal, colors

styledEcho fgColor, colRed, "Test"
styledEcho bgColor, colBlue, "Test"
