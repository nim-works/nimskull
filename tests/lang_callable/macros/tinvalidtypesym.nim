discard """
errormsg: "type expected, but symbol 'MyType' has no type."
"""

import std/macros

macro foobar(name) =
  let sym = genSym(nskType, "MyType")

  result = quote do:
    type
      `name` = `sym`

foobar(MyAlias)
