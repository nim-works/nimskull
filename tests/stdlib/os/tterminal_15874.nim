discard """
  cmd: "nim c --app:console $file"
  action: "compile"
"""

import std/terminal

writeStyled("hello", {styleBright})
