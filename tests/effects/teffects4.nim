discard """
  errormsg: "type mismatch"
  line: 26
"""

type
  TObj = object {.pure, inheritable.}
  TObjB = object of TObj
    a, b, c: string
    fn: proc (): int {.tags: [ReadIOEffect].}



proc q() {.tags: [IoEffect].} =
  discard

proc effectfulWrite(s: string) {.tags: [WriteIOEffect].} =
  discard

proc raiser(): int =
  effectfulWrite "arg"
  if true:
    q()

var o: TObjB
o.fn = raiser
