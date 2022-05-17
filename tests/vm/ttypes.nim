discard """
  description: '''Tests for `vmgentypes` too make sure cyclic types and size
                  calculation works'''
  action: compile
"""

# To make sure that type generation and size calculation worked, every
# location is accessed

block array_alias:
  type
    Arr1 = array[2, O]
    Arr2 = array[2, O]
    O = object
      s: seq[Arr1]
    A = object
      t: Arr1
      t2: Arr2

  static:
    var x = A()
    doAssert x.t[1].s.len == 0
    x.t[1].s.add([O(), O()])
    doAssert x.t[1].s[0][0].s.len == 0
    x.t2 = x.t

block tuple_issue1:
  type
    Tup = tuple[o: O]
    O = object
      s: seq[Tup]
    A = object
      t: Tup

  static:
    var x = A()
    doAssert x.t.o.s.len == 0
    x.t.o.s.add((O(),))
    doAssert x.t.o.s[0].o.s.len == 0

block tuple_issue2:
  type
    Tup = tuple[o: O, i: int]
    Tup2 = tuple[o: O, i: int]
    O = object
      s: seq[Tup]
    A = object
      t: Tup
      t2: Tup2

  static:
    var x = A()
    doAssert x.t.o.s.len == 0
    x.t.o.s.add((O(), 1))
    doAssert x.t.o.s[0].o.s.len == 0
    x.t2 = x.t


block tuple_issue3:
  type
    Tup = tuple[o: O, i: int]
    Tup2 = tuple[se: seq[Tup], i: int]
    O = object
      s: seq[Tup2]
    A = object
      t: Tup

  static:
    var x = A()
    doAssert x.t.o.s.len == 0
    x.t.o.s.add((@[(O(), 1)], 1))
    doAssert x.t.o.s[0].se[0].o.s.len == 0


#[
# XXX: Tuple recursion via procs is not prevented leading to a compiler
#      crash (infinite recursion, it seems).
block:
  type
    Tu = tuple[a: B]
    Obj = seq[Tu]
    B = proc(x: Obj)
]#

block self_recursion:
  type A = object
    x: ref A

  static:
    var a = A()
    a.x = new(A)
    doAssert a.x.x == nil

block recursion_through_other:
  type
    A = object
      x: ptr B
    B = object
      a: A

  static:
    var a = A()
    var b = B()
    a.x = addr b
    doAssert a.x.a.x == nil

block non_object_cycle:
  type
    A = object
      x: ptr A
    B = object
      a: ptr A

  static:
    var b = B()
    var a = A()
    b.a = addr a
    b.a.x = b.a

block:
  type
    A = object
      x: ptr B
      y: ptr C
      z: ptr D
    B = object
      a: A
    C = object
      b: B
    D = object
      b: B
      c: C

  static:
    var a = A()
    var b = B()
    var c = C()
    var d = D()
    a.x = addr b
    a.y = addr c
    a.z = addr d
    a.x.a = a
    a.y.b = b
    a.z.b = b
    a.z.c = c