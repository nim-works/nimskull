discard """
  description: "Tests for the serialization/deserialization"
  targets: "c cpp !js"
"""

# knownIssue: fails with JS backend due to code-gen issues as well as due to
#             `transf` working differently regarding closures when compiling
#             for the JS backend (even for code being transformed for the VM)

# This test file tests four things:
# * VM representation -> PNode tree representation (via `deserialize`)
# * PNode tree representation -> VM representation (via `serialize` and `putIntoReg`)
# * loading of `const` value into VM memory (`serialize` and `vmgen`)
# * is the backend able to embed the constant?

template testCase(n, ty, setup, testCode) =
  const c = setup # `deserialize` happens here

  macro m(n: static[ty]) =
    testCode

  m(c) # `serialize` and `putIntoReg` happen here

  static:
    let n: ty = c # Usage in static context
    testCode

  when not declared(disableBackend):
    block:
      # compile-time/run-time border
      let n: ty = c
      testCode


# XXX: the `const` sym is erroneously inlined somwhere before reaching the
#      back-end, so this case doesn't really test if the backend can handle
#      a `skConst` of closure type
block closure_with_single_capture:

  proc mkClosure(): proc(): int =
    # Closure with a single capture
    var x = 1
    result = proc(): int =
      x

  testCase(v, proc(): int {.closure.}, mkClosure()):
    doAssert v() == 1

# XXX: the issue mentioned above also applies for this test case
block closure_with_multiple_captures:

  proc mkClosure2(): proc(): int =
    var
      x = 1
      y = 2
      z = 3
    result = proc(): int =
      x + y + z

  testCase(v, proc(): int {.closure.}, mkClosure2()):
    doAssert v() == 6


block nil_closure:
  testCase(v, proc(): int {.closure.}, nil):
    doAssert v == nil

block nil_closure_in_object:
  type A = object
    c: proc() {.closure.}

  testCase(v, A, A(c: nil)):
    doAssert v.c == nil

block non_nil_closure_in_object:
  type A = object
    c: proc(): int {.closure.}

  proc mkClosure(): proc(): int =
    var x = 1
    result = proc(): int =
      x

  # Fails on the c/cpp and VM backend (what about JS?) since closures with
  # non-nil environments are not supported
  const disableBackend = true
  testCase(v, A, A(c: mkClosure())):
    doAssert v.c() == 1

block non_nil_proc_in_object:
  type Obj = object
    a: int
    b: proc(): int {.nimcall.}

  proc p(): int = 2

  testCase(v, Obj, Obj(a: 1, b: p)):
    doAssert v.a == 1
    doAssert v.b() == 2


block empty_object:
  type Empty = object

  testCase(v, Empty, Empty()):
    discard

block nil_pointer:
  testCase(v, pointer, nil):
    doAssert v == nil

block nil_int_ptr:
  testCase(v, ptr int, nil):
    doAssert v == nil

#[
# Currently no way to represent a `ptr int` with PNodes
block non_nil_int_ptr:
  testCase(v, ptr int, (var x = 1; addr x)):
    doAssert v[] == 1
]#


block nil_ref:
  testCase(v, ref int, nil):
    doAssert v == nil

#[
# Currently no way to represent a `ref int` with PNodes
block non_nil_int_ref:
  testCase(v, ref int, (var x = new int; x[] = 1; x)):
    doAssert v[] == 1
]#

#[
# vmcompilerserdes supports this, but ref is rejected for const
block obj_ref:
  type Ty = ref object
    a: string

  testCase(v, Ty, Ty(a: "a")):
    doAssert v.a == "a"
]#

#[
# vmcompilerserdes supports this, but ref is rejected for const
block supertype_ref:
  type
    A = object of RootObj
      x: int
    B = ref object of A
      y: int
    C = object
      a: ref A

  testCase(v, C, C(a: B(x: 1, y: 2))):
    doAssert v.a.x == 1
    doAssert B(v.a).y == 2
]#

block named_tuple_value:
  type Tuple = tuple
    a, b: seq[int]

  # Also tests seqs

  testCase(v, Tuple, (@[1, 2, 3], @[4, 5, 6, 7])):
    doAssert v.a == @[1, 2, 3]
    doAssert v.b == @[4, 5, 6, 7]


block complex_case_object:

  type Tb = object
    x: string

  # multiple record-cases at the top-level, nested record-cases
  type A = object
    a: int
    case b: range[0..5]
    of 0..2:
      c: string
      d: seq[int]
      case e: bool
      of true:
        f: int
      of false:
        g: seq[string]
    of 3:
      h: seq[float]
      case i: range[0..2]
      of 0:
        discard
      of 1:
        j: float64
      of 2:
        k: Tb
      l: seq[Tb]
    of 4..5:
      m: uint8
      n: bool
    o: float
    p: pointer

    case q: bool
    of false:
      r: array[10, int]
    of true:
      s: set[uint8]
      case t: bool
      of false:
        discard
      of true:
        u: Tb
        case v: bool
        of false:
          discard
        of true:
          x: int

  testCase(v, A, A(a: 1, o: 0.5, q: true, s: {2'u8})):
    doAssert v.a == 1
    doAssert v.o == 0.5
    doAssert v.q == true
    doAssert v.s == {2'u8}

block object_with_inheritance:

  type
    C {.inheritable.} = object
      a: int
      b: string
      c: seq[float]

    B = object of C
      # empty object

    A = object of B
      d: string
      e: bool

  macro test(v: static A) =
    doAssert v.a == 2
    doAssert v.b == "b"
    doAssert v.c == [0.0, 1.0]
    doAssert v.d == "d"
    doAssert v.e == true

  # XXX: can't use `testCase`, since objects with inheritance are rejected for
  #      consts
  # setup values in reverse order
  test(A(e: true, d: "d", c: @[0.0, 1.0], b: "b", a: 2))

block object_inheritance_case:

  type
    C {.inheritable.} = object
      a: int
      case b: bool
      of false:
        c: string
      of true:
        d: float
      e: seq[int]

    B = object of C
      case f: range[0..2]
      of 0:
        g: int8
      of 1:
        h: bool
      of 2:
        i: string

    A = object of B
      j: C
      case k: bool
      of false:
        l: seq[bool]
      of true:
        m: set[uint8]
      n: seq[string]

  macro test(v: static A) =
    doAssert v.b == true
    doAssert v.d == 1.0
    doAssert v.m == {1'u8}

  test(A(b: true, d: 1.0, k: true, m: {1'u8}))

block:
  type Ty[T] = object
    x: int
    when T is int: # the `when` is turned into an nkRecList which
                  # vmcompilerserdes as well as type creation has to handle
                  # correctly
      y: int
      z: T

  testCase(v, Ty[int], Ty[int](x: 1, y: 2, z: 3)):
    doAssert v.x == 1
    doAssert v.y == 2
    doAssert v.z == 3