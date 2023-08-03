discard """
  targets: "c js !vm"
"""

# knowIssue: testutils imports ``std/os``, which is not yet supported by the
#            VM

import std/hashes
from stdtest/testutils import disableVm, whenVMorJs

block hashes:
  block hashing:
    var dummy = 0.0
    doAssert hash(dummy) == hash(-dummy)

  # "VM and runtime should make the same hash value (hashIdentity)"
  block:
    const hi123 = hashIdentity(123)
    doAssert hashIdentity(123) == hi123

  # "VM and runtime should make the same hash value (hashWangYi1)"
  block:
    const wy123 = hashWangYi1(123)
    doAssert wy123 != 0
    doAssert hashWangYi1(123) == wy123


  # "hashIdentity value incorrect at 456"
  block:
    doAssert hashIdentity(456) == 456

  # "hashWangYi1 value incorrect at 456"
  block:
    when Hash.sizeof < 8:
      doAssert hashWangYi1(456) == 1293320666
    else:
      doAssert hashWangYi1(456) == -6421749900419628582

block empty:
  var
    a = ""
    b = newSeq[char]()
    c = newSeq[int]()
    d = cstring""
    e = "abcd"
  doAssert hash(a) == 0
  doAssert hash(b) == 0
  doAssert hash(c) == 0
  doAssert hash(d) == 0
  doAssert hashIgnoreCase(a) == 0
  doAssert hashIgnoreStyle(a) == 0
  doAssert hash(e, 3, 2) == 0

block sameButDifferent:
  doAssert hash("aa bb aaaa1234") == hash("aa bb aaaa1234", 0, 13)
  doAssert hash("aa bb aaaa1234") == hash(cstring"aa bb aaaa1234")
  doAssert hashIgnoreCase("aA bb aAAa1234") == hashIgnoreCase("aa bb aaaa1234")
  doAssert hashIgnoreStyle("aa_bb_AAaa1234") == hashIgnoreCase("aaBBAAAa1234")

block smallSize: # no multibyte hashing
  let
    xx = @['H', 'i']
    ii = @[72'u8, 105]
    ss = "Hi"
  doAssert hash(xx) == hash(ii)
  doAssert hash(xx) == hash(ss)
  doAssert hash(xx) == hash(xx, 0, xx.high)
  doAssert hash(ss) == hash(ss, 0, ss.high)

block largeSize: # longer than 4 characters
  let
    xx = @['H', 'e', 'l', 'l', 'o']
    xxl = @['H', 'e', 'l', 'l', 'o', 'w', 'e', 'e', 'n', 's']
    ssl = "Helloweens"
  doAssert hash(xxl) == hash(ssl)
  doAssert hash(xxl) == hash(xxl, 0, xxl.high)
  doAssert hash(ssl) == hash(ssl, 0, ssl.high)
  doAssert hash(xx) == hash(xxl, 0, 4)
  doAssert hash(xx) == hash(ssl, 0, 4)
  doAssert hash(xx, 0, 3) == hash(xxl, 0, 3)
  doAssert hash(xx, 0, 3) == hash(ssl, 0, 3)

proc main() =
  doAssert hash(0.0) == hash(0)
  # bug #16061
  doAssert hash(cstring"abracadabra") == 97309975
  doAssert hash(cstring"abracadabra") == hash("abracadabra")

  when sizeof(int) == 8 or defined(js):
    block:
      var s: seq[Hash]
      for a in [0.0, 1.0, -1.0, 1000.0, -1000.0]:
        let b = hash(a)
        doAssert b notin s
        s.add b
    when defined(js):
      doAssert hash(0.345602) == 2035867618
      doAssert hash(234567.45) == -20468103
      doAssert hash(-9999.283456) == -43247422
      doAssert hash(84375674.0) == 707542256
    else:
      doAssert hash(0.345602) == 387936373221941218
      doAssert hash(234567.45) == -8179139172229468551
      doAssert hash(-9999.283456) == 5876943921626224834
      doAssert hash(84375674.0) == 1964453089107524848
  else:
    doAssert hash(0.345602) != 0
    doAssert hash(234567.45) != 0
    doAssert hash(-9999.283456) != 0
    doAssert hash(84375674.0) != 0

  block: # bug #16555
    proc fn(): auto =
      # avoids hardcoding values
      var a = "abc\0def"
      var b = a.cstring
      result = (hash(a), hash(b))
      doAssert result[0] != result[1]
    when not defined(js):
      doAssert fn() == static(fn())
    else:
      # xxx this is a tricky case; consistency of hashes for cstring's containing
      # '\0\' matters for c backend but less for js backend since such strings
      # are much less common in js backend; we make vm for js backend consistent
      # with c backend instead of js backend because FFI code (or other) could
      # run at CT, expecting c semantics.
      discard

  block: # hash(object)
    type
      Obj = object
        x: int
        y: string
      Obj2[T] = object
        x: int
        y: string
      Obj3 = object
        x: int
        y: string
      Obj4 = object
        case t: bool
        of false:
          x: int
        of true:
          y: int
        z: int
      Obj5 = object
        case t: bool
        of false:
          x: int
        of true:
          y: int
        z: int

    proc hash(a: Obj2): Hash = hash(a.x)
    proc hash(a: Obj3): Hash = hash((a.x,))
    proc hash(a: Obj5): Hash =
      case a.t
      of false: hash(a.x)
      of true: hash(a.y)

    doAssert hash(Obj(x: 520, y: "Nim")) != hash(Obj(x: 520, y: "Nim2"))
    doAssert hash(Obj2[float](x: 520, y: "Nim")) == hash(Obj2[float](x: 520, y: "Nim2"))
    doAssert hash(Obj2[float](x: 520, y: "Nim")) != hash(Obj2[float](x: 521, y: "Nim2"))
    doAssert hash(Obj3(x: 520, y: "Nim")) == hash(Obj3(x: 520, y: "Nim2"))

    doAssert hash(Obj4(t: false, x: 1)) == hash(Obj4(t: false, x: 1))
    doAssert hash(Obj4(t: false, x: 1)) != hash(Obj4(t: false, x: 2))
    doAssert hash(Obj4(t: false, x: 1)) != hash(Obj4(t: true, y: 1))

    doAssert hash(Obj5(t: false, x: 1)) != hash(Obj5(t: false, x: 2))
    doAssert hash(Obj5(t: false, x: 1)) == hash(Obj5(t: true, y: 1))
    doAssert hash(Obj5(t: false, x: 1)) != hash(Obj5(t: true, y: 2))

  block: # hash(ref|ptr|pointer)
    var a: array[10, uint8]
    # disableVm:
    whenVMorJs:
      # pending fix proposed in https://github.com/nim-lang/Nim/issues/15952#issuecomment-786312417
      discard
    do:
      doAssert a[0].addr.hash != a[1].addr.hash
      doAssert cast[pointer](a[0].addr).hash == a[0].addr.hash

  block: # hash(ref)
    type A = ref object
      x: int
    let a = A(x: 3)
    disableVm: # xxx Error: VM does not support 'cast' from tyRef to tyPointer
      let ha = a.hash
      doAssert ha != A(x: 3).hash # A(x: 3) is a different ref object from `a`.
      a.x = 4
      doAssert ha == a.hash # the hash only depends on the address

  block: # hash(proc)
    proc fn(a: int): auto = a*2
    doAssert fn isnot "closure"
    doAssert fn is (proc)
    const fn2 = fn
    let fn3 = fn
    whenVMorJs: discard
    do:
      doAssert hash(fn2) == hash(fn)
      doAssert hash(fn3) == hash(fn)

  block: # hash(closure)
    proc outer() =
      var a = 0
      proc inner() = a.inc
      doAssert inner is "closure"
      let inner2 = inner
      whenVMorJs: discard
      do:
        doAssert hash(inner2) == hash(inner)
    outer()

static: main()
main()
