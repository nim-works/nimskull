discard """
  description: "Tests for the std/experimental/results module"
  targets: "c js vm"
  knownIssue.vm: "code-gen issues"
"""

# This file was based on the ``test.nim`` file
# from https://github.com/disruptek/badresults

import experimental/results
import std/options
import std/macros

# Translation helpers
template check(cond: bool, msg: string = "") =
  {.line.}:
    doAssert cond, msg

macro check(msg: string, body: untyped) =
  result = newStmtList()

  body.expectKind nnkStmtList
  for n in body:
    result.add(newCall(bindSym"check", n, msg))

template expect(e: typed, body: untyped) =
  doAssertRaises e, body

template test(m, body) =
  block:
    body


proc main() =
  type R = Result[int, string]

  ## Basic usage, producer
  func works(): R = R.ok(42)
  func works2(): R = result.initSuccess(42)
  func fails(): R = R.err("dummy")
  func fails2(): R = result.initFailure("dummy")

  ## Basic usage, consumer
  let
    rOk = works()
    rOk2 = works2()
    rErr = fails()
    rErr2 = fails2()

  test "Basic operations":
    check "":
      rOk.isOk
      rOk2.isOk
      rOk.get() == 42
      (not rOk.isErr)
      rErr.isErr
      rErr2.isErr

  test "Exception on access (1)":
    let va = try: discard rOk.error; false except: true
    check va, "not an error, should raise"

  test "Exception on access (2)":
    let vb = try: discard rErr.value; false except: true
    check vb, "not an value, should raise"

  test "Mutate":
    var x = rOk

    x.initFailure("failed now")

    check x.isErr

    check rOk.valueOr(50) == rOk.value
    check rErr.valueOr(50) == 50

  test "Comparisons":
    check (works() == works2())
    check (fails() == fails2())
    check (works() != fails())

  test "Custom exceptions":
    type
      AnEnum = enum
        anEnumA
        anEnumB
      AnException = ref object of ValueError
        v: AnEnum

    func toException(v: AnEnum): AnException = AnException(v: v)

    func testToException(): int =
      try:
        var r = Result[int, AnEnum].err(anEnumA)
        get r
      except AnException:
        42

    check testToException() == 42

  test "Adhoc string rendering of exception":
    type
      AnEnum2 = enum
        anEnum2A
        anEnum2B

    func testToString(): int =
      try:
        var r = Result[int, AnEnum2].err(anEnum2A)
        get r
      except ResultError[AnEnum2]:
        42

    check testToString() == 42

  test "Dollar-based string rendering of an error":
    type
      AnEnum3 = enum
        anEnum3A
        anEnum3B

    func `$`(e: AnEnum3): string =
      case e
      of anEnum3A: "first"
      of anEnum3B: "second"

    proc testToString2(): int =
      try:
        var r = Result[int, AnEnum3].err(anEnum3B)
        get r
      except ResultError[AnEnum3] as e:
        check e.msg == "Result isErr: second"
        42

    check testToString2() == 42

  test "Void Results":
    type VoidRes = Result[void, int]

    func worksVoid(): VoidRes = VoidRes.ok()
    func worksVoid2(): VoidRes = result.initSuccess()
    func failsVoid(): VoidRes = VoidRes.err(42)
    func failsVoid2(): VoidRes = result.initFailure(42)

    let
      vOk = worksVoid()
      vOk2 = worksVoid2()
      vErr = failsVoid()
      vErr2 = failsVoid2()

    check vOk.isOk
    check vOk2.isOk
    check vErr.isErr
    check vErr2.isErr

    vOk.get()

  test "Functional operations":
    type
      FloatRes = Result[float, int]
      StrRes = Result[string, int]
      Res2 = Result[float, string]

    let
      vOk = FloatRes.ok(1.0)
      vErr = FloatRes.err(1)

    func toStr(x: int): string = $x
    func toStr(x: float): string = $x

    let
      vOk2 = vOk.map(toStr)
      vErr2 = vErr.mapErr(toStr)
      vOk3 = vOk.mapErr(toStr)
      vErr3 = vErr.map(toStr)

    check vOk2.value == "1.0"
    check vOk3.value == 1.0
    check vErr2.error == "1"
    check vErr3.error == 1

    let
      vS = vOk.filter(proc(x: float): bool = x == 1.0)
      vN = vOk.filter(proc(x: float): bool = x == 0.0)
      vN2 = vErr.filter(proc(x: float): bool = true)
      eS = vErr.filterErr(proc(x: int): bool = x == 1)
      eN = vErr.filterErr(proc(x: int): bool = x == 0)
      eN2 = vOk.filterErr(proc(x: int): bool = true)

    check vS.get() == 1.0
    check vN.isNone
    check vN2.isNone
    check eS.get() == 1
    check eN.isNone
    check eN2.isNone

    check vOk2.take == "1.0"
    check vErr2.takeErr == "1"

    expect ResultError[void]:
      discard vOk3.takeErr

    expect ResultError[int]:
      discard vErr3.take

  test "Symbol resolution":
    type
      Q = Result[float, int]
      R = Result[void, int]
      S = Result[float, ref IOError]

    let
      a = Q.ok 5.3
      b = ok R
      c = Q.err 5
      d = R.err 3
      e = S.ok 5.3
      f = S.err: IOError.newException "uh-oh"

    proc `$`(s: S): string =
      case s.isOk
      of true:  $s.get
      of false: "BIO:" & $s.error.name & "/" & s.error.msg

    check get(a) == 5.3
    check get(e) == 5.3
    get(b)
    check c.error == 5
    check d.error == 3
    check f.error is ref IOError
    check $a == "Ok(5.3)"
    check $b == "Ok()"
    check $e == "5.3"
    check $f == "BIO:IOError/uh-oh"
    check unsafeGet(a) == 5.3
    check unsafeGet(e) == 5.3
    check get(c, 3.2) == 3.2
    check get(f, 3.2) == 3.2
    expect IOError:
      discard get f

  test "Tuple values":
    type
      Tup = tuple[a, b: int]
      R = Result[Tup, int]

    var r = R.ok (1, 2)

    check r.unsafeGet() == (1, 2)
    check r.get() == (1, 2)
    check r.value() == (1, 2)

    r.value().a = 3

    check r.unsafeGet() == (3, 2)
    check r.get() == (3, 2)
    check r.value() == (3, 2)

# TODO: use the VM target once it's available in testament
#static: main()
main()