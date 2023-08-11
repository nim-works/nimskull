discard """
  targets: "c js vm"
  description: "
    Ensure that enum values stored as usigned integers are properly read as
    such
  "
"""

type
  EnumA = enum
    enuA_a = 0
    enuA_b = high(uint8)

  EnumB = enum
    enuB_a = 0
    enuB_b = high(uint16)

doAssert sizeof(EnumA) == sizeof(uint8)
doAssert sizeof(EnumB) == sizeof(uint16)

proc test() =
  # globals could interfere with the test, so use a procedure in order to
  # ensure that locals are used. In addition, assign the enum values to
  # variables so that the ``ord`` calls are not folded away
  var val1 = enuA_b
  doAssert ord(val1) == int high(uint8)

  var val2 = enuB_b
  doAssert ord(val2) == int high(uint16)

  # the VM has to narrow the value after loading it from a memory
  # location, so also test that case by accessing an array

  var arr1 = [enuA_b]
  doAssert ord(arr1[0]) == int high(uint8)

  var arr2 = [enuB_b]
  doAssert ord(arr2[0]) == int high(uint16)

test()

block compile_time_run_time_boundary:
  # make sure that enum values stored as unsigned integers properly cross the
  # compile-time/run-time boundary
  proc get[T](): T = high(T)

  block uint8_value:
    const
      folded   = (enuA_b,)       # the VM is not used
      computed = (get[EnumA](),) # ``vmcompilerserdes`` is used

    var val = folded
    doAssert val[0] == enuA_b
    val = computed
    doAssert val[0] == enuA_b

  block uint16_value:
    const
      folded   = (enuB_b,)       # the VM is not used
      computed = (get[EnumB](),) # ``vmcompilerserdes`` is used

    var val = folded
    doAssert val[0] == enuB_b
    val = computed
    doAssert val[0] == enuB_b