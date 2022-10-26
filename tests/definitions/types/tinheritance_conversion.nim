discard """
  cmd: "nim c --hints=on $file"
  joinable: false
"""


type
  Base {.inheritable.} = object
    field: int

  Derived = object of Base
    field2: int
    field3: int
  Derived2 = object of Base

block: # Value tests
  proc test(args: varargs[Base]) =
    for x in args:
      assert x.field == 0

  proc test2(base: var Base) = base.field = 400
  proc test3(base: Base) = discard
  var a: Derived = Derived(Base())
  a = Derived(Base(Derived2()))
  test(
    Derived(), #[tt.Hint
    ^ Implicit conversion: Receiver 'Base' will not receive fields of sub-type 'Derived' [ImplicitObjConv] ]#
    Base(),
    Derived() #[tt.Hint
    ^ Implicit conversion: Receiver 'Base' will not receive fields of sub-type 'Derived' [ImplicitObjConv] ]#
  )

  a.field2 = 300
  test2(a)
  assert a.field == 400
  assert a.field2 == 300
  var b = Derived2(field: 800)
  b.test2()
  assert b.field == 400
  b.test3() #[tt.Hint
  ^ Implicit conversion: Receiver 'Base' will not receive fields of sub-type 'Derived2' [ImplicitObjConv] ]#



block: # Ref tests
  type
    Base = ref object of RootObj
      field: int
    Derived = ref object of Base
      field2: int
    Derived2 = ref object of Base

  var a: Base = Derived()
  assert Derived(a) is Derived
  doAssertRaises(ObjectConversionDefect): discard Derived2(a)[]
  doAssertRaises(ObjectConversionDefect): discard Base(Derived2()).Derived
  assert Base(Derived()) is Base
  assert Derived2(Base(Derived2())) is Derived2
  assert Derived(Base(Derived())) is Derived

block: # Pointer tests
  template make(t: typedesc): ptr t =
    let res = createU(t)
    res[] = t()
    res
  var a: ptr Base = make(Derived)
  assert (ptr Derived)(a) is (ptr Derived)
  doAssertRaises(ObjectConversionDefect): discard (ptr Derived2)(a)[]
  doAssertRaises(ObjectConversionDefect):
    var a = make(Derived2)
    discard (ptr Derived)((ptr Base)(a))
  assert (ptr Base)(make(Derived)) is (ptr Base)
  assert (ptr Derived2)((ptr Base)(make(Derived2))) is (ptr Derived2)
  assert (ptr Derived)((ptr Base)(make(Derived))) is (ptr Derived)
