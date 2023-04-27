discard """
description: "Structured echo message comparison"
nimoutformat: "sexp"
cmd: "nim c --filenames=canonical --msgFormat=sexp --hint:successx:off $file"
action: compile
nimout: '''
(IntEchoMessage :severity Trace :msg "test message" :location nil :reportInst _ :reportFrom _)
'''
"""

static:
  echo "test message"
