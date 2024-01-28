discard """
  description: '''
    Regression test for constant-evaluated seqs of aggregate types resulting
    in memory leaks
  '''
  targets: "c"
  matrix: "-d:useMalloc"
  valgrind: true
"""

# caused no problems:
var s1 = static: @[1, 2]
# caused a leak:
var s = static: @[(1, 2)]
