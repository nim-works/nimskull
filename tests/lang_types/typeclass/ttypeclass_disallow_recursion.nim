discard """
  description: "Type Classes cannot reference themselves"
  errormsg: "illegal recursion in type 'Foo'"
"""

type
  Foo = int | Foo
