discard """
  description: '''Tests to make sure access violations are properly detected
                  and reported'''
  nimoutFormat: sexp
  cmd: "nim check --msgFormat=sexp --filenames=canonical --hints:off $options $file"
  action: reject
"""

import maccess_checks

type
  ObjectA = object
    x: int
    y: float
  ObjectB = object
    x: int
    y: float

template access(origTyp, asTyp: type; expr) =
  static:
    var x: origTyp
    let p {.inject.} = cast[ptr asTyp](addr x)
    let v = expr

static:
  var p: ptr int = nil
  discard p[] #[tt.Error
          ^ (SemVmNilAccess) ]#

static:
  let x = localPtr(string)
  let v = x[] #[tt.Error
          ^ (SemVmAccessOutOfBounds) ]#

static:
  let p = cast[ptr int](1)
  let v = p[] #[tt.Error
          ^ (SemVmAccessOutOfBounds) ]#


access(uint32, int32): p[] #[tt.Error
                       ^ (SemVmAccessTypeMismatch) ]#

access(int64, uint64): p[] #[tt.Error
                       ^ (SemVmAccessTypeMismatch) ]#

access(float, int): p[] #[tt.Error
                    ^ (SemVmAccessTypeMismatch) ]#

access(char, int): p[] #[tt.Error
                   ^ (SemVmAccessTypeMismatch) ]#

access(ObjectA, ObjectB): p[] #[tt.Error
                          ^ (SemVmAccessTypeMismatch) ]#

static:
  var x: int32
  cast[ptr byte](cast[int](addr x) + 1)[] = 1 #[tt.Error
                                      ^ (SemVmAccessNoLocation) ]#

objectTest((int32, int)): # unnamed tuple overlay
  let v = p[][1] #[tt.Error
            ^ (SemVmAccessTypeMismatch) ]#


objectTest(tuple[x: int32, y: int]): # named tuple overlay
  let v = p.y #[tt.Error
          ^ (SemVmAccessTypeMismatch) ]#

objectTest(array[4, int32]):
  let v = p[][1] #[tt.Error
            ^ (SemVmAccessTypeMismatch) ]#


static:
  var o = Object(b: true)
  let p = cast[ptr tuple[x: bool, y: int]](addr o.b)
  let v = p.y #[tt.Error
          ^ (SemVmAccessNoLocation) ]#

static:
  var s = newSeq[int](4)
  let p = cast[ptr array[5, int]](addr s[0])
  # XXX: should report a `SemVmAccessOutOfBounds` but actually reports a
  #      `SemVmAccessTypeMismatch`. See `tsafety_checks_issues2`
  let v = p[][5] #[tt.Error
            ^ (SemVmAccessTypeMismatch) ]#

# TODO: the tests using `testSource` might violate the parameter aliasing
#       rules, but there's currently no other way to force an invalid source
#       location. Once the VM supports `alloc` and `dealloc`, those should be
#       used instead
proc testSource(x: var seq[string], y: ptr string, m: range[0..2]) =
  type Obj = object
    v: string
    arr: array[1, string]

  # resizes the seq's underlying buffer, making `y` a dangling pointer:
  x.add("")
  var o: Obj

  # note: the validation of the source currently happens as part of
  # `opcWrDeref`, `opcWrArr`, and `opcWrObj`, which is why the position
  # information for the error is a bit confusing.

  case m
  of 0:
    # writing to object field
    o.v = y[] #[tt.Error
    ^ (SemVmAccessOutOfBounds) ]#
  of 1:
    # writing through pointer
    var p = addr o.v
    p[] = y[] #[tt.Error
    ^ (SemVmAccessOutOfBounds) ]#
  of 2:
    # writing to array
    o.arr[0] = y[] #[tt.Error
        ^ (SemVmAccessOutOfBounds) ]#

static:
  var s = newSeq[string](1)
  testSource(s, addr s[0], 0)

static:
  var s = newSeq[string](1)
  testSource(s, addr s[0], 1)

static:
  var s = newSeq[string](1)
  testSource(s, addr s[0], 2)