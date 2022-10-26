discard """
  targets: "c cpp"
  labels: "generic inheritance overload proc"
  description: '''
  . From https://github.com/nim-lang/Nim/issues/5570
    inheriting from specialized generic object and calling generic proc
    generate wrong field type
  . From https://github.com/nim-lang/Nim/issues/5602
    Wrong type substituted for generic parameter
'''
"""



import typetraits, module_with_generics

block:

  type
    Foo[T] = object of RootObj
    Bar[T] = object of Foo[seq[T]]

  proc p[T](f: Foo[T]): T =
    return result


  var bar: Bar[float]
  var genericOfBar = p( bar )

   # the bug was: p(s) should return seq[float], but returns float instead
  doAssert genericOfBar is seq[float]
  doAssert genericOfBar.len == 0


block:
  # Test overloading and code generation when
  # downcasting is required for generic types:
  var d = makeDerived(10)
  setBaseValue(d, 20)


