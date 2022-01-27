discard """
description: "Structured echo message comparison"
nimoutformat: "sexp"
cmd: "nim c --filenames=canonical --msgFormat=sexp $file"
action: compile
nimout: '''
(IntEchoMessage :msg "test message")
'''
"""

static:
  echo "test message"
