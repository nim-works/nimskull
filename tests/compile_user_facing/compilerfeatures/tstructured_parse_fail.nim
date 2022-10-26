discard """
description: "Structured parser error report"
nimoutformat: "sexp"
cmd: "nim c --filenames=canonical --msgFormat=sexp $file"
action: reject
nimout: '''
(ParInvalidIndentation :severity Error :found "[EOF]" :location (_ 12 0))
'''
"""

static:
