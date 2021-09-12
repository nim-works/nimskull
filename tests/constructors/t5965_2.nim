discard """
  errormsg: "Invalid field assignment '2'"
  file: "t5965_2.nim"
  line: 10
"""

type Foo = object
  a: int

discard Foo(a: 1, 2)
