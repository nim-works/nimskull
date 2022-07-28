discard """
  description: "Tests to make sure branch changing works correctly"
  targets: "c cpp"
  matrix: "--gc:refc; --gc:arc; --gc:orc"
"""

# XXX: remove this together with arc/orc related workarounds once
# `tbranch_reset_arc_orc_issue.nim` works
const isArcOrOrc = defined(gcArc) or defined(gcOrc)

block:

  # This is also a test for generic variant objects

  type A[T] = object
    case kind: bool
    of false:
      a: T
    of true:
      b: T


  # variant with simple types (no destructors exist)
  when not isArcOrOrc:
    block:
      # ARC/ORC fails this test as branch resets are not injected here
      var a = A[int](kind: false, a: 1)
      a.kind = true
      doAssert a.b == 0

  # variant with type using destructors
  block:
    var a = A[string](kind: false, a: "a")
    a.kind = true
    doAssert a.b == ""


block update_discrim_same_of_branch:

  type B = object
    s: seq[string]
    case kind: bool
    of false, true:
      str: string


  var b = B(kind: false)
  b.str = "a"
  b.kind = true # Event though both true and false refer to the same
                # of branch, the branch is reset
  doAssert b.str == ""


block non_case_object_field:

  type A = object
    s: seq[string]
    case kind: bool
    of false:
      str: string
    of true:
      i: int


  var a: A
  a.s = @["a"]

  a.kind = true # change
  a.i = 1
  doAssert a.s[0] == "a"

  a.kind = false # change
  a.str = "str"
  doAssert a.s[0] == "a"

  a.kind = false # update, but the active branch is not changed
  doAssert a.s[0] == "a"
  doAssert a.str == "str"

  a.kind = true # change
  a.i = 2
  doAssert a.s[0] == "a"


block sub_case_object:
  # Change the branch for a record case inside another record case

  type T = object
    fix: string # fix to force arc/orc to generate branch reset logic

    case k: bool
    of false:
      a: int

    of true:
      case k2: bool
      of false:
        b: int
      of true:
        c: int

      other: int

  var t = T(k: false, a: 1)
  t.k = true # change active branch
  doAssert t.k2 == false
  doAssert t.b == 0
  doAssert t.other == 0

  t.other = 3

  t.k2 = true # change active branch for the sub-case
  doAssert t.c == 0
  doAssert t.other == 3
  t.c = 2

  t.k = false
  doAssert t.a == 0


block case_in_parent_obj:
  # Change the branch where the rec-case is in a parent object

  type A = ref object of RootObj # the `ref object` here is intentional
    case kind: bool
    of false:
      x: int
    of true:
      y: int

  type B = ref object of A
    z: int


  var a = B(kind: false, x: 1, z: 2)
  a.kind = true
  doAssert a.y == 0
  doAssert a.z == 2


block discr_update_in_field:
  # Update the discriminator of a field

  type
    A = object
      fix: string # fix to force arc/orc to generate branch reset logic

      case kind: bool
      of false:
        x: int
      of true:
        y: int

    B = object
      a: A

  var b = B(a: A(kind: false, x: 1))
  b.a.kind = true
  doAssert b.a.y == 0