discard """
  targets: "c js vm"
  matrix: "--hints=on"
  knownIssue.vm: '''
    * the VM reports a run-time error when accessing a down-converted non-ref
      object
    * conversion checks for pointer types are not implemented
  '''
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
  # XXX: ^^ it's questionable that this is allowed. The VM reports a run-time
  #      access violation here, since a location of type ``Base`` cannot be
  #      accessed as if it were a ``Derived`` (only the other way around)
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
  var
    derived = Derived()
    derived2 = Derived2()
    a: ptr Base = addr derived
  assert (ptr Derived)(a) is (ptr Derived)
  doAssertRaises(ObjectConversionDefect): discard (ptr Derived2)(a)[]
  doAssertRaises(ObjectConversionDefect):
    var a = addr derived2
    discard (ptr Derived)((ptr Base)(a))
  assert (ptr Base)(addr derived) is (ptr Base)
  assert (ptr Derived2)((ptr Base)(addr derived2)) is (ptr Derived2)
  assert (ptr Derived)((ptr Base)(addr derived)) is (ptr Derived)
