discard """
description: "Structured echo message comparison"
nimoutformat: "sexp"
matrix: "--filenames=canonical --msgFormat=sexp"
action: compile
nimout: '''
(IntEchoMessage :msg "test message")
'''
"""

static:
  echo "test message"
