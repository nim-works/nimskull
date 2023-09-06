discard """
  errormsg: "cannot inherit from a type that is not an object type"
  line: 7
"""
# bug #1691
type
  Foo = ref object of Foo
