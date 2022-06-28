discard """
matrix: "--gc:refc; --gc:orc"
labels: "error_compile iterators table top_level"
description: '''
  . From https://github.com/nim-lang/Nim/pull/16374
    Codegen bug with Table mvalues in loop and ARC & ORC GC
  . Small example that uses Table.mvalues within a loop fails to
    compile the generated C with --gc:arc and --gc:orc flags
  . Only an issue with --gc:arc and --gc:orc. All others
    (markAndSweep, boehm, go, none, regions, refc) work fine.
  . Seems like the Table value type being a seq is important
    it compiles if I change it to an int, or even an array[0..1, int].
  . Putting it into procs seems to work.
'''
"""

block:
  iterator mvalues(t: var seq[seq[int]]): var seq[int] =
    yield t[0]

  var t: seq[seq[int]]

  while false:
    for v in t.mvalues:
      discard

  proc ok =
    while false:
      for v in t.mvalues:
        discard

  ok()

block:
  iterator mvalues(t: var seq[seq[int]]): lent seq[int] =
    yield t[0]

  var t: seq[seq[int]]

  while false:
    for v in t.mvalues:
      discard

  proc ok =
    while false:
      for v in t.mvalues:
        discard

  ok()