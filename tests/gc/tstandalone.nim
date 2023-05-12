discard """
  matrix: "--os:standalone --gc:none"
  exitcode: 1
  output: "value out of range"
  knownIssue: "`--gc:none` is currently unsupported"
"""

type
  rangeType = range[0..1]

var
  r: rangeType = 0
  i = 2

r = rangeType(i)
