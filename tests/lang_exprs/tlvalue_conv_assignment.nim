discard """
  labels: "subtyping conversion distinct"
  description: '''
    Assigning to the result of an lvalue conversion must modify the underlying
    location
  '''
  targets: "c js vm"
"""

type
  Base = object of RootObj
  A = object of Base
  B = object of A

  Obj[T] = object
    field: T

block ref_and_ptr_conversion:
  proc test[A, B, C](def: C) =
    block down_conv_local:
      var x = def
      # assignment to down-converted local:
      B(x) = nil
      doAssert x == nil

      x = def

      # assignment to down-converted local (nested):
      A(B(x)) = nil
      doAssert x == nil

    block up_conv_local:
      var x = A(def)
      # assignment to up-converted local:
      B(x) = nil
      doAssert x == nil

      x = A(def)

      # assignment to up-converted local (nested):
      C(B(x)) = nil
      doAssert x == nil

    block down_conv_field:
      # assignment to down-converted field:
      var x = Obj[C](field: def)
      B(x.field) = nil
      doAssert x.field == nil

      # assignment to down-converted field (nested):
      x = Obj[C](field: def)
      A(B(x.field)) = nil
      doAssert x.field == nil

    block up_conv_field:
      # assignment to up-converted field:
      var x = Obj[C](field: def)
      B(x.field) = nil
      doAssert x.field == nil

      # assignment to up-converted field (nested):
      x = Obj[C](field: def)
      C(B(x.field)) = nil
      doAssert x.field == nil

  # conversions involving refs:
  test[ref Base, ref A, ref B](new(B))

  # conversions involving ptrs:
  when not defined(vm): # XXX: not yet supported by ``vmgen``
    var obj = B()
    test[ptr Base, ptr A, ptr B](addr obj)

block distinct_conversion:
  proc test[T, Base](a: T, b: Base) =
    block to_base:
      var x = a
      Base(x) = b
      doAssert Base(x) == b

    block from_base:
      var x = b
      T(x) = a
      doAssert x == Base(a)

    block to_base_field:
      var x = Obj[T](field: a)
      Base(x.field) = b
      doAssert Base(x.field) == b

    block from_base_field:
      var x = Obj[Base](field: b)
      T(x.field) = a
      doAssert x.field == Base(a)

  type DInt = distinct int

  when not defined(vm):
    # XXX: crashes the compiler
    test(DInt(1), 2)