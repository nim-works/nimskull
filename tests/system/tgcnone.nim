discard """
  matrix: "--gc:none -d:useMalloc"
  knownIssue: "`--gc:none` is currently unsupported"
"""
# bug #15617
let x = 4
doAssert x == 4
