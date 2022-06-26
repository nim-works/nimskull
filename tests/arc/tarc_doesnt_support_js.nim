discard """
  targets: "c js"
  matrix: "--gc:arc"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/16033
      nim js --gc:arc gives bad error: undeclared identifier: '+!'
    . Expected:
      either make --gc:arc a noop (and code should work) or
      give error --gc:arc not supported with js backend
    . Testament could also be made smart about this and skip
      js --gc:arc combination, although this isn't trivial to do it cleanly.
  '''
"""

when defined js:
  doAssert not compileOption("gc", "arc")
else:
  doAssert compileOption("gc", "arc")