discard """
  errormsg: "Invalid field assignment '2'"
  file: "tincorrect_arg_syntax_2.nim"
  line: 13
  description: '''
    . From https://github.com/nim-lang/Nim/issues/5965
      nim crashes when instancing with incorrect arg syntax
  '''
"""
type Foo = object
  a: int

discard Foo(a: 1, 2)