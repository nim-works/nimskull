discard """
  errormsg: "type mismatch: got <seq[Obj]> but expected 'seq[float]'"
  line: 10
"""

type
  Obj = object
  Alias = Obj

var x: seq[float] = newSeq[Alias]()
