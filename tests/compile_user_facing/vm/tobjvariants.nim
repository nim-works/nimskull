discard """
  description: "Various VM tests for object variants"
  action: compile
"""

block:
  # Tests `variantFields` in the case where a rec-case is surrounded by fields

  type A = object
    f1: char
    f2: uint
    case a: bool # the content of this record-case is not important
    of false, true: discard
    f3: string
    f4: seq[int]

  static:
    var val = A(a: false, f1: 'a', f2: 1, f3: "f3", f4: @[1])

    var other = val # perform a copy
    # Make sure that all fields were copied. If they weren't, `variantFields`
    # is likely broken
    doAssert val.f1 == 'a'
    doAssert val.f2 == 1
    doAssert val.f3 == "f3"
    doAssert val.f4 == [1]

block multiple_empty_branches:
  static:
    type A = object
      case a: range[0..3]
      of 0:
        discard
      of 1:
        b: int
      of 2:
        c: string
      of 3:
        discard

    var other: A
    var val = A(a: 1, b: 2)
    # change branch
    val.a = 0

    other = val
    doAssert other.a == 0

    # change back
    val.a = 1
    doAssert val.b == 0
    val.b = 10

    other = val
    doAssert other.a == 1 and other.b == 10

    # change to other branch
    val.a = 2
    doAssert val.c == ""
    val.c = "test"

    other = val
    doAssert other.a == 2 and other.c == "test"


block multiple_empty_branches:
  # A non-empty record case inside a branch that's followed by an empty branch
  # without fields in-between

  type A = object
    case a: bool
    of false:
      case b: bool
      of false:
        c: string
      of true: discard # doesn't matter what's in this branch
    of true:
      discard # this branch must be empty

  static:
    var val = A(a: false, b: false, c: "test")
    var other = val
    doAssert other.c == "test"


block:
  # Test a variant object with nested record cases in the
  # respective parents' last branch with an extra field following
  # after the top level branch.

  static:
    type A = object
      case a: bool
      of false: discard
      of true:
        case b: bool
        of false: discard
        of true:
          case c: bool
          of false: discard
          of true:
            d: int
      e: int

    var val = A(a: true, b: true, c: true, d: 1, e: 2)

    var other = val
    doAssert other.d == 1
    doAssert other.e == 2


block:
  type
    E = enum
      eA
      eB
      eC
    A = object
      case kind: E
      of eA: x: int
      else: y: int

  static:
    # assign a static discriminator value that maps to the 'else' branch
    var a = A(kind: eC)

    block:
      let d = if true: eB else: eA
      # set discriminator to a dynamic value that maps to the 'else' branch
      a.kind = d

    block:
      let d = if true: eA else: eB
      # set discriminator to a dynamic value that maps to an 'of' branch
      a.kind = d

# XXX: not a VM specific test. Could be moved into language spec
block sub_discr_assign:
  type A = object
    case kind: bool
    of false:
      case k2: bool:
      of false, true:
        a: int
    of true: discard

  static:
    # test to make sure that assigning to a discriminator wrapped in a
    # `nkCheckedFieldAccess` works
    var a: A
    a.k2 = true # checked field access

# XXX: not specific too the VM (rather vmgen). Could be moved to a general
#      backend test
block generic_variant:
  type A[T] = object
    case kind: bool
    of false:
      a: T
    of true:
      b: T

  static:
    # test to make sure that discriminator setup/assignment works if the
    # discriminator is part of a generic type
    var a = A[int](kind: false, a: 1)
    a.kind = true
    doAssert a.b == 0

# XXX: general backend test
block assign_in_base_type:
  type
    A = ref object of RootObj # `ref object` is on purpose
      case kind: bool
      of false:
        a: int
      of true:
        b: int
    B = ref object of A

  static:
    var b = new(B)
    b.kind = true
    b.b = 10 # `b` is not a field of `B`

# XXX: general backend/language test
block non_first_default:
  type A = object
    case kind: bool
    of true: # The first branch is not the default branch
      x: string
    of false:
      y: int

  static:
    var a = A(kind: true)
    a.x = "1"
    a.kind = false # switch branch
    a.y = 2
    a.kind = true # switch back

    var b: A # no explicit initialization
    b.y = 0