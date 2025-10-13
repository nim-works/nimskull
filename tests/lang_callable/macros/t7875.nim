discard """
  nimout: "var mysym: MyType[float32]"
  joinable: false
"""

import macros

type
  MyType[T] = object

macro foobar(): untyped =
  let floatSym = bindSym"float32"

  result = quote do:
    var mysym: MyType[`floatSym`]

  echo result.repr

foobar()
