discard """
description: "Structured parser error report"
nimoutformat: "sexp"
matrix: "--filenames=canonical --msgFormat=sexp"
action: reject
nimout: '''
(ParInvalidIndentation :severity Error :found "[EOF]" :location (_ 12 0))
'''
"""

static:
