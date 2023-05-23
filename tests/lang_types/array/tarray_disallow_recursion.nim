discard """
  description: "An array type cannot contain itself"
  errormsg: "illegal recursion in type 'Foo'"
"""

type
  Foo = array[5, Foo]
