discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/7416
    Ambiguous call error when it's not
  . Since IntFoo is a concrete type, it should be preferred over a generic
    type object|tuple. It will work if you define type IntFoo = object
    for example.
  . It works since Nim 0.20.0
'''
"""

type
  Foo[T] = object
  IntFoo = Foo[int]

proc bar(b: object|tuple) : int = return 1
proc bar(b: IntFoo) : int = return 2

var f: IntFoo
doAssert bar(f) == 2