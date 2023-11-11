discard """
  errormsg: "len(x) must be less than 32768"
  line: 7
"""

type Typ = object
  case x: range[0..100_000]
  of 0:
    discard
  else:
    discard