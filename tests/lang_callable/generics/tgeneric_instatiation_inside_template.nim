discard """
action: compile
description: '''
  . From https://github.com/nim-lang/Nim/issues/5926
    "Cannot instantiate" error when template uses generic type
  . It works in Nim 1.0.6
'''
"""

type
  SomeObj[T] = object

template useSomeObj[T]() =
  var retObj: SomeObj[T]

useSomeObj[void]()
useSomeObj[int]()


type
  Data*[T] = object
    x: T

template test*[T](xxx: T) =
  let data = Data[T](x: xxx)

test(1)

