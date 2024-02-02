discard """
targets: "c js vm"
knownIssue.js: '''
  The code generator rejects 'of' branch ranges > 2^16
'''
output:
'''
Not found!
Found!
1
compiles for 1
i am always two
default for 3
set is 4 not 5
array is 6 not 7
default for 8
an identifier
OK
OK
OK
a
yyy
dd
'''
"""


block arrayconstr:
  const md_extension = [".md", ".markdown"]

  proc test(ext: string) =
    case ext
    of ".txt", md_extension:
      echo "Found!"
    else:
      echo "Not found!"

  test(".something")
  # ensure it's not evaluated at compile-time:
  var foo = ".markdown"
  test(foo)


converter toInt(x: char): int =
  x.int
block t8333:
  case 0
  of 'a': echo 0
  else: echo 1


block emptyset_when:
  proc whenCase(a: int) =
    case a
    of (when compiles(whenCase(1)): 1 else: {}): echo "compiles for 1"
    of {}: echo "me not fail"
    of 2: echo "i am always two"
    of []: echo "me neither"
    of {4,5}: echo "set is 4 not 5"
    of [6,7]: echo "array is 6 not 7"
    of (when compiles(neverCompilesIBet()): 3 else: {}): echo "compiles for 3"
    #of {},[]: echo "me neither"
    else: echo "default for ", a

  whenCase(1)
  whenCase(2)
  whenCase(3)
  whenCase(4)
  whenCase(6)
  whenCase(8)


block setconstr:
  const
    SymChars: set[char] = {'a'..'z', 'A'..'Z', '\x80'..'\xFF'}

  proc classify(s: string) =
    case s[0]
    of SymChars, '_': echo "an identifier"
    of {'0'..'9'}: echo "a number"
    else: echo "other"

  classify("Hurra")



block tduplicates:
  type Kind = enum A, B
  var k = A

  template reject(b) =
    static: doAssert(not compiles(b))

  reject:
      var i = 2
      case i
      of [1, 1]: discard
      else: discard

  reject:
      var i = 2
      case i
      of 1, { 1..2 }: discard
      else: discard

  reject:
      var i = 2
      case i
      of { 1, 1 }: discard
      of { 1, 1 }: discard
      else: discard

  reject:
      case k
      of [A, A]: discard

  var i = 2
  case i
  of { 1, 1 }: discard
  of { 2, 2 }: echo "OK"
  else: discard

  case i
  of { 10..30, 15..25, 5..15, 25..35 }: discard
  else: echo "OK"

  case k
  of {A, A..A}: echo "OK"
  of B: discard


block tcasestm:
  type
    Tenum = enum eA, eB, eC

  var
    x: string = "yyy"
    y: Tenum = eA
    i: int

  case y
  of eA: echo "a"
  of eB, eC: echo "b or c"

  case x
  of "Andreas", "Rumpf": echo "Hallo Meister!"
  of "aa", "bb": echo "Du bist nicht mein Meister"
  of "cc", "hash", "when": discard
  of "will", "it", "finally", "be", "generated": discard

  var z = case i
    of 1..5, 8, 9: "aa"
    of 6, 7: "bb"
    elif x == "Ha":
      "cc"
    elif x == "yyy":
      echo x
      "dd"
    else:
      "zz"

  echo z
  #OUT ayyy

  let str1 = "Y"
  let str2 = "NN"
  let a = case str1:
    of "Y": true
    of "N": false
    else:
      echo "no good"
      quit("quitting")

  proc toBool(s: string): bool =
    case s:
    of "": raise newException(ValueError, "Invalid boolean")
    elif s[0] == 'Y': true
    elif s[0] == 'N': false
    else: "error".quit(2)


  let b = "NN".toBool()

  doAssert(a == true)
  doAssert(b == false)

  block:
    # bug https://github.com/nim-lang/nim/issues/7407
    let bstatic = "N".toBool()
    doAssert(bstatic == false)

  var bb: bool
  doAssert(not compiles(
    bb = case str2:
      of "": raise newException(ValueError, "Invalid boolean")
      elif str.startsWith("Y"): true
      elif str.startsWith("N"): false
  ))

  doAssert(not compiles(
    bb = case str2:
      of "Y": true
      of "N": false
  ))

  doAssert(not compiles(
    bb = case str2:
      of "Y": true
      of "N": raise newException(ValueError, "N not allowed")
  ))

  doAssert(not compiles(
    bb = case str2:
      of "Y": raise newException(ValueError, "Invalid Y")
      else: raise newException(ValueError, "Invalid N")
  ))


  doAssert(not compiles(
    bb = case str2:
      of "Y":
        raise newException(ValueError, "Invalid Y")
        true
      else: raise newException(ValueError, "Invalid")
  ))


  doAssert(not compiles(
    bb = case str2:
      of "Y":
        "invalid Y".quit(3)
        true
      else: raise newException(ValueError, "Invalid")
  ))

#issue #11552

proc positiveOrNegative(num: int): string =
  result = case num
  of (low(int)+2) .. -1:
    "negative"
  of 0:
    "zero"
  else:
    "impossible"

#issue #11551

proc negativeOrNot(num: int): string =
    result = case num
    of low(int) .. -1:
      "negative"
    else:
      "zero or positive"

doAssert negativeOrNot(-1) == "negative"
doAssert negativeOrNot(10000000) == "zero or positive"
doAssert negativeOrNot(0) == "zero or positive"

########################################################
# issue #13490
import strutils
func foo(input: string): int =
  try:
    parseInt(input)
  except:
    return

func foo2(b, input: string): int =
  case b:
    of "Y":
      for c in input:
        result =  if c in '0'..'9': parseInt($c)
                  else: break
    of "N":
      for c in input:
        result =  if c in '0'..'9': parseInt($c)
                  else: continue
    else: return

doAssert(foo("3") == 3)
doAssert(foo("a") == 0)
doAssert(foo2("Y", "a2") == 0)
doAssert(foo2("Y", "2a") == 2)
doAssert(foo2("N", "a3") == 3)
doAssert(foo2("z", "2") == 0)

block float_slice_list:
  ## Tests for slice lists of float values
  proc p(f: float): int =
    case f
    of 5.0, -2.0 .. 1.2, 6.0: 0 # slice-list with gaps
    of 1.5: 1
    of 20.0 .. 32.0: 2
    else: 3

  # direct matches
  doAssert p(5.0) == 0
  doAssert p(-2.0) == 0
  doAssert p(1.2) == 0
  doAssert p(6.0) == 0
  doAssert p(1.5) == 1
  doAssert p(20.0) == 2
  doAssert p(32.0) == 2

  # in ranges
  doAssert p(0.1) == 0
  doAssert p(-1.0) == 0
  doAssert p(1.1) == 0
  doAssert p(1.6) == 3
  doAssert p(25.0) == 2