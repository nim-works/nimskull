discard """
  description: '''
    Generic constraints for enum and tuple fail to resolve (ambigouus call)
    if explicit generic parameter is used. When called without explicit
    parameter (type inferred from the argument), code compiles correctly.
  '''
  knownIssue: "enum/tuple generic constraint fails to resolve (ambiguous)"
"""

# once fixed combine with type class specifications

proc store[T: enum](target: T) = discard
proc store[T: tuple](target: T) = discard

proc aux1[T](obj: T) = store(obj)

## aux1 can run and compile correctly
aux1((1, 2))

proc aux2[T](obj: T) = store[T](obj)

## aux2 fails compilation
aux2((1, 2))
