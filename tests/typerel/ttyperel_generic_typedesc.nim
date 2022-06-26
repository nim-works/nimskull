discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/7734
    Generics in typedesc: required type Foo but of type: type Foo
  . Generics passed to typedesc requires full generics definition.
'''
"""
type
  Foo[T: SomeFloat] = object
    learning_rate: T

  Model = object
    weight: int


proc optimizer[M; T: SomeFloat](model: M, _: typedesc[Foo], learning_rate: T): Foo[T] =
  result.learning_rate = learning_rate

let a = Model(weight: 1)
let opt = a.optimizer(Foo, 10.0)