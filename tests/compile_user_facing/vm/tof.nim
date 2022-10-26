discard """
  description: "Tests for the 'of' operator inside the VM"
  action: compile
"""

# TODO: once testament has a VM target, move these tests to the language spec

block ref_of:
  type
    Base = ref object of RootObj
    Sub1 = ref object of Base
    Sub2 = ref object of Base
    Unrelated = ref object

  # These were copied verbatim from the `runnableExamples`
  # of `system.of`
  static:
    var base: Base = Sub1() # downcast
    doAssert base of Base # generates `CondTrue` (statically true)
    doAssert base of Sub1
    doAssert base isnot Sub1
    doAssert not (base of Sub2)

    base = Sub2() # re-assign
    doAssert base of Sub2
    doAssert Sub2(base) != nil # upcast

    # XXX: the VM aborts directly without raising a defect (this is
    #      technically valid since Defects are not required to be catchable)
    #doAssertRaises(ObjectConversionDefect):
    #  discard Sub1(base)

    var sub1 = Sub1()
    doAssert sub1 of Base
    doAssert sub1.Base of Sub1

  static:
    let baseNil = Base(nil)
    doAssert baseNil of Base
    doAssert not (baseNil of Sub1)

block object_of:
  type
    Base = object of RootObj
      x: int
    B = object of Base
    C = object of Base

  static:
    var a = Base()
    var b = B()
    var c = C()
    doAssert a of Base
    doAssert b of Base
    doAssert b of B
    doAssert not (c of B)