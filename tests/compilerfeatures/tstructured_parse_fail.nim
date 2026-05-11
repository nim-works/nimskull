discard """
description: "Structured parser error report"
nimoutformat: "sexp"
cmd: "nim c --msgFormat=sexp $file"
action: reject
disabled: windows
nimout: '''
(ParInvalidIndentation :location ("tstructured_parse_fail.nim" 13 0) :reportFrom ("parser.nim" _ _) :reportInst ("parser.nim" _ _) :severity Error)
'''
"""

static:
