discard """
  targets: "!js"
  matrix: "--app:console"
  action: "compile"
"""

import terminal

writeStyled("hello", {styleBright})
