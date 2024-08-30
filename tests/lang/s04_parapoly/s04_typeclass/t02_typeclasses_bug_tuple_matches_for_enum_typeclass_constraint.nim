discard """
  description: '''
    Regression test where generic constraints for enum and tuple fail to
    resolve (ambigouus call) if an explicit generic parameter is used. When
    called without an explicit parameter (the type is inferred from the
    argument), code compiles correctly.
  '''
"""

# TODO: once fixed combine with type class specifications??

proc store[T: enum](target: T) = discard
proc store[T: tuple](target: T) = discard

proc aux1[T](obj: T) = store(obj)

## aux1 can run and compile correctly
aux1((1, 2))

proc aux2[T](obj: T) = store[T](obj)

## aux2 fails compilation
aux2((1, 2))
