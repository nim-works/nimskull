discard """
  targets: "c js vm"
  description: '''
    Regression test for a C code generator bug triggered indirectly by
    iterator inlining
  '''
"""

# it's important for `Base` to be both a generic ref type
type
  Base[T] = ref object of RootObj
  Sub     = ref object of Base[int]

iterator iter(x: Base[int]): int =
  # the iterator's body doesn't matter
  discard

proc get(x: Sub): lent Sub =
  # a procedure that returns the borrowed parameter
  x

proc test() =
  let v = Sub()
  # an implicit up-conversion is inserted by the compiler for the iterator
  # call's argument, which combined with the value coming from dereferencing
  # a `lent` view resulted in a C code generator error
  for it in iter(get(v)):
    discard

test()