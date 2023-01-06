discard """
  errormsg: "identifier expected, but found 'keyword of'"
  line: 11
"""

type
  TMyEnum = enum enA, enU, enO
  TMyCase = object
    case e: TMyEnum
    of enA:
    of enU: x, y: int
    of enO: a, b: string
