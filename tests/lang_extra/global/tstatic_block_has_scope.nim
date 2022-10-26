discard """
  errormsg: "undeclared identifier: 'a'"
  line: 21
  description: '''
  . From https://github.com/nim-lang/Nim/issues/5958
    Compiler crash with static block
  . This code is not valid - static blocks introduce their own scope,
    so there should be a proper compile-time error here.
    To introduce a global compile-time variable, you can use the
    compileTime pragma.

  . From https://github.com/nim-lang/Nim/issues/6036
    incomplete code generation when using a compile time variable
    at run time (undeclared identifier)
  '''
"""

static:
  var a = 1

echo a

