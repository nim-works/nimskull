discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/18859
    Weird static[T](T) type produced by type expression in
    generic type definition
'''
"""

import macros

macro symFromDesc(T: typedesc): untyped =
  let typ = getType(T)
  typ[1]

template produceType(T: typedesc): untyped =
  type
    XT = object
      x: symFromDesc(T)

  XT

type
  X[T] = produceType(T)

var x: X[int]

