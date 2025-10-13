discard """
  description: '''
    Regression test for a C code generator bug with `seq` constructions that
    resulted in a heap corruption at run-time.
    Reduced from https://github.com/nim-works/nimskull/issues/1117"
  '''
  targets: "c"
  matrix: "-d:useMalloc"
  valgrind: true
"""

type Sequence[T] = seq[T]

# the compile-time-evaluated value must have a generic instantiation as the type
# in order to reproduce the issue
let x = static:
  var large: array[1024, int]
  # the sequence element must be larger than ``sizeof(seq[T])``, no heap
  # corruption occurs otherwise
  Sequence[array[1024, int]](@[large])