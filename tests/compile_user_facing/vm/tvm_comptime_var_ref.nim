discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/11637
    {.compileTime.} var ref are not updated properly for implicit result
  . When in the vm var ref parameters are not properly updated and the VM
    keeps refering to the initial address.
'''
"""
type Foo = ref object
  val: int

proc `+`(a, b: Foo): Foo =
  Foo(val: a.val + b.val)

proc `*`(a: Foo, b: int): Foo =
  Foo(val: a.val * b)

proc `+=`(a: var Foo, b: Foo) =
  a = Foo(
    val: a.val + b.val
  )

proc foobar(a, b, c: Foo): tuple[bar, baz, buzz: Foo] =

  let foo = a + b + c
  result.bar = foo * 2

  result.baz = foo * 3
  result.buzz = result.baz

  result.buzz += a * 10000
  result.baz += b
  result.buzz += b


block: # Compile-Time
  let
    a {.compileTime.} = Foo(val: 1)
    b {.compileTime.} = Foo(val: 2)
    c {.compileTime.} = Foo(val: 3)
    r {.compileTime.} = foobar(a, b, c)

  static:
    doAssert r.bar.val == 12
    doAssert r.baz.val == 20
    doAssert r.buzz.val == 10020

####################################

block: # Run-time
  let
    a = Foo(val: 1)
    b = Foo(val: 2)
    c = Foo(val: 3)
    r = foobar(a, b, c)

  # Expected values
  doAssert r.bar.val == 12
  doAssert r.baz.val == 20
  doAssert r.buzz.val == 10020

