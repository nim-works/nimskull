discard """
  errormsg: "type mismatch"
  line: 19
"""

type
  TObj = object {.pure, inheritable.}
  TObjB = object of TObj
    a, b, c: string
    fn: proc (): int {.tags: [].}

proc effectfulWrite(s: string) {.tags:[WriteIoEffect].} =
  discard

proc raiser(): int {.tags: [TObj, WriteIoEffect].} =
  effectfulWrite "arg"

var o: TObjB
o.fn = raiser
