discard """
knownIssue: "https://github.com/nim-works/nimskull/pull/27"
description: '''
disabled this test because the ast is being screwed up and we get gensyms when we shouldn't

the error reporting in this test is out of order due to nkError refactoring, it
should be reexamined each time there is a failure as the failure might be a
false positive and the new behaviour is in fact correct.

Currently, semobjconstr related error reporting happens eagerly hence the out
of order reporting, where bad2, bad6, bad7, and Foo's instantiation are
reported early/out of order.
'''
cmd: '''nim check --hints:off $file'''
action: reject
nimout: '''
tundeclared_field.nim(49, 13) Error: undeclared field: 'bad2' for type tundeclared_field.A [type declared in tundeclared_field.nim(47, 8)]
tundeclared_field.nim(63, 19) Error: undeclared field: 'bad6' for type tundeclared_field.B [type declared in tundeclared_field.nim(58, 8)]
tundeclared_field.nim(65, 15) Error: undeclared field: 'bad7' for type tundeclared_field.B [type declared in tundeclared_field.nim(58, 8)]
tundeclared_field.nim(69, 13) Error: cannot instantiate Foo [type declared in tundeclared_field.nim(68, 8)]
got: <typedesc[float]>
but expected: <T: SomeInteger>
tundeclared_field.nim(44, 13) Error: undeclared field: 'bad1' for type tundeclared_field.A [type declared in tundeclared_field.nim(41, 8)]
tundeclared_field.nim(44, 12) Error: expression 'a.bad1' has no type (or is ambiguous)
tundeclared_field.nim(49, 12) Error: Invalid object constructor: 'A_452984836(bad2: 0)'
tundeclared_field.nim(49, 12) Error: expression 'A_452984836(bad2: 0)' has no type (or is ambiguous)
tundeclared_field.nim(55, 5) Error: undeclared field: 'bad3' for type tundeclared_field.A [type declared in tundeclared_field.nim(52, 8)]
tundeclared_field.nim(61, 13) Error: undeclared field: 'bad4' for type tundeclared_field.B [type declared in tundeclared_field.nim(58, 8)]
tundeclared_field.nim(61, 12) Error: expression 'b.bad4' has no type (or is ambiguous)
tundeclared_field.nim(62, 5) Error: undeclared field: 'bad5' for type tundeclared_field.B [type declared in tundeclared_field.nim(58, 8)]
tundeclared_field.nim(63, 18) Error: Invalid object constructor: 'B[int](bad6: 0)'
tundeclared_field.nim(63, 18) Error: expression 'B[int](bad6: 0)' has no type (or is ambiguous)
tundeclared_field.nim(65, 14) Error: Invalid object constructor: 'Bi(bad7: 0)'
tundeclared_field.nim(65, 14) Error: expression 'Bi(bad7: 0)' has no type (or is ambiguous)
'''
"""

#[
xxx in future work, generic instantiations (e.g. `B[int]`) should be shown with their instantiation instead of `tundeclared_field.B`,
maybe using TPreferedDesc.preferResolved or preferMixed
]#
# line 39
block:
  type A = object
    a0: int
  var a: A
  discard a.bad1

block:
  type A = object
    a0: int
  var a = A(bad2: 0)

block:
  type A = object
    a0: int
  var a: A
  a.bad3 = 0

block:
  type B[T] = object
    b0: int
  var b: B[int]
  discard b.bad4
  b.bad5 = 0
  var b2 = B[int](bad6: 0)
  type Bi = B[int]
  var b3 = Bi(bad7: 0)

block:
  type Foo[T: SomeInteger] = object
  var a: Foo[float]
