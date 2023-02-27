discard """
description: "Structured parser error report"
nimoutformat: "sexp"
cmd: "nim c --filenames=canonical --msgFormat=sexp $file"
action: reject
nimout: '''
(ParInvalidIndentation :location ("tests/compilerfeatures/tstructured_parse_fail.nim" 12 0) :reportFrom ("parser.nim" _ _) :reportInst ("parser.nim" _ _) :severity Error)
'''
"""

static:
