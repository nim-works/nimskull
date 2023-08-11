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
