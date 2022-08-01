discard """
joinable: false
target: "c !cpp js"
labels: "array"
description: '''
  . From https://github.com/nim-lang/Nim/issues/1669
    Regression: array using high
    ___

  . From https://github.com/nim-lang/Nim/issues/3899
    Wrong bounds check using template [] to access array in a const object
  . If c is declared as a var, the code works correctly.
    ___

  . From https://github.com/nim-lang/Nim/issues/6675
    Wrong indices in arrays not starting with 0
  . Fixed by
    https://github.com/nim-lang/Nim/commit/0f5261e9711c3fd57241874963bd5e45b11ed65e
    ___

  . From https://github.com/nim-lang/Nim/issues/6852
    Negative range for array makes no sense but compiles, and last invalid
    line causes segmentation fault.
    Semcheck negative array length: https://github.com/nim-lang/Nim/pull/7518
    ___

  . From https://github.com/nim-lang/Nim/issues/6853
    Impossible to create an empty const array
    ___

  . From https://github.com/nim-lang/Nim/issues/7153
    Unsigned integers could not be used as array indexes
  . Array indexing does not have to be bound by the "ordinal type" constraint.
    ___

  . From https://github.com/nim-lang/Nim/issues/7818
     inconsistent internal representation of generic objects array construction
  . I use macro to avoid object slicing
  . This is not a macro bug, but array construction bug ,  see #7712 and #7637

    ___

  . From https://github.com/nim-lang/Nim/issues/8049
    Converter applied when it should not be
  . Fixed by `introduce precise string '[]', '[]=' accessors`
    https://github.com/nim-lang/Nim/commit/df4d5b77a14b3b84f3ff37b3741dd039c9e21ce4
    ___

  . From https://github.com/nim-lang/Nim/issues/8316
    BUG: "varargs[string, $]" calls $ n^2 times instead of n times (n=len(varargs))
    ___

  . From https://github.com/nim-lang/Nim/issues/12466
    Crash in sameTree() with uint literals #12466
    ___

  . From https://github.com/nim-lang/Nim/pull/17705
    Fix array's high & low return type for empty arrays #17705
  . a is of type range 0..-1, and the compilers checks for
    range.min <= x <= range.max (and 0 < x < -1 can't work)
  . It is intentional that reverse ranges are empty, I don't think it's desired
    to change that. With this "fix", it wouldn't be guaranteed that an index
    that is in the range I is also a valid index for array[I, T] anymore.
    The real issue here is that low and high can't possibly be valid indices
    for array[0, T]. I can't think of a good fix for that:
      * changing the return type of low/high doesn't work, since the index type
        doesn't need to be an integer type (unless array[0, T] is special cased)
      * using -1 .. 0 for array[0, T] makes no sense (and neither would 0..0),
        because -1 and 0 are no valid indices
    ___

  . From https://github.com/nim-lang/Nim/issues/18643
    Accessing an empty array sometimes works #18643

'''
"""

block tarray:

  type
    TMyArray = array[0..2, int]
    TMyRecord = tuple[x, y: int]
    TObj = object
      arr: TMyarray

  block arrayToopenArray:
     proc sum(a: openarray[int]): int =
           result = 0
           var i = 0
           while i < len(a):
             inc(result, a[i])
             inc(i)
     doAssert sum([1, 2, 3, 4]) == 10
     doAssert sum([]) == 0

  block leftoverTests:
    proc getPos(r: TMyRecord): int =
      result = r.x + r.y
    doAssert getPos( (x: 5, y: 7) ) == 12

    const myData = [[1,2,3], [4, 5, 6]]
    doAssert myData[0][2] == 3

  block issue_1669:
    # Regression with arrays.high
    let letters = ["a" , "b", "c", "d"]
    var foundVar: array[0..letters.high, bool]

    doAssert foundVar.len == letters.len


  block issue_6853:
    # make sure empty arrays are assignable (bug #6853)

    const arr1: array[0, int] = []
    const arr2 = []
    let arr3: array[0, string] = []
    var arr4: array[0, string] = []

    doAssert(arr1.len == 0)
    doAssert(arr2.len == 0)
    doAssert(arr3.len == 0)
    doAssert(arr4.len == 0)

  block issue_68523:
    # Negative array length is not allowed (#6852)
    doAssert(
    not compiles(
     block:
      var arr: array[-1, int]
      ))

  block issue_106:
    # SIGSEGV when array is used as var return type
    proc mul(a, b: TMyarray): TMyArray =
      result = a
      for i in 0..len(a)-1:
        result[i] = a[i] * b[i]

    var
      x, y: TMyArray
      o: TObj

    proc arrayId(x: var TMyArray): var TMyArray = x
    proc arrayGet(x: var TObj): var TMyArray = x.arr

    x = [4, 5, 6]
    y = x
    o.arr = mul(x, y)


    doAssert repr(arrayId(x)) == "[4, 5, 6]"
    doAssert repr(mul(x, y)) == "[16, 25, 36]"
    doAssert repr(arrayGet(o)) == "[16, 25, 36]"

block tarraycons:

  type
    TEnum = enum
      eA, eB, eC, eD, eE, eF

  const
    myMapping: array[TEnum, array[0..1, int]] = [
      eA: [1, 2],
      eB: [3, 4],
      [5, 6], # Inferred as eC
      eD: [0: 8, 1: 9],
      eE: [0: 8, 9],
      eF: [2, 1: 9]
    ]

  doAssert myMapping[eC][1] == 6

block tarraycons_ptr_generic:
  type
    Fruit = object of RootObj
      name: string
    Apple = object of Fruit
    Banana = object of Fruit


  block iter_dereference_ptrs_1:
    var
        apple = Apple(name: "apple")
        banana = Banana(name: "banana")
        fruit = Fruit(name: "Fruit")
    let fruits = [apple.addr, banana.addr, fruit.addr]


    var names: seq[string]
    for it in fruits:
      names.add( it.name )
    doAssert names == [ "apple" , "banana", "Fruit"]


  block iter_dereference_ptrs_2:
    type
      Vehicle[T] = object of RootObj
        tire: T
      Car[T] = object of Vehicle[T]
      Bike[T] = object of Vehicle[T]

    var
      bike = Bike[int](tire: 2)
      car = Car[int](tire: 4)
      vehicle = Vehicle[int](tire: 3)
    let vehicles = [bike.addr, car.addr, vehicle.addr]

    var tires : seq[int]
    for it in vehicles:
      tires.add( it.tire )
    doAssert  tires == [2, 4 , 3]


  block iter_ref_generic:
    type
      Book[T] = ref object of RootObj
        cover: T
      Hard[T] = ref object of Book[T]
      Soft[T] = ref object of Book[T]

    var
        book = Book[string](cover: "none")
        hardBook = Hard[string](cover: "skin")
        softBook = Soft[string](cover: "paper")
    let books = [book, hardBook, softBook]


    var covers : seq[string]
    for it in books:
      covers.add( it.cover )
    doAssert covers == [ "none" , "skin" , "paper" ]

block tarraylen:
  var a: array[0, int]
  doAssert a.len == 0
  doAssert array[0..0, int].len == 1
  doAssert array[0..0, int]([1]).len == 1
  doAssert array[1..1, int].len == 1
  doAssert array[1..1, int]([1]).len == 1
  doAssert array[2, int].len == 2
  doAssert array[2, int]([1, 2]).len == 2
  doAssert array[1..3, int].len == 3
  doAssert array[1..3, int]([1, 2, 3]).len == 3
  doAssert array[0..2, int].len == 3
  doAssert array[0..2, int]([1, 2, 3]).len == 3
  doAssert array[-2 .. -2, int].len == 1
  doAssert([1, 2, 3].len == 3)
  doAssert([42].len == 1)


type ustring = distinct string
converter toUString(s: string): ustring = ustring(s)
# converter is only allowed at top level

block tarrayindx:

  block issue_7153:
    # Unsigned integers could not be used as array indexes
    const UnsignedConst = 1024'u
    type
      SomeObject = object
        s1: array[UnsignedConst, uint32]

    var obj: SomeObject
    doAssert obj.s1[0] == 0
    doAssert obj.s1[0u] == 0


  block issue_8049:
    # converter applied when it shouldn't be
    proc `[]`(s: ustring, i: int): ustring = s
    doAssert "abcdefgh"[1..2] == "bc"
    doAssert "abcdefgh"[1..^2] == "bcdefg"

block troof:

  block:

    var a = @[1, 2, 3, 4]
    doAssert a[1 .. ^1] ==  @[2, 3, 4]
    doAssert a[^1] == 4
    doAssert a[^2] == 3
    doAssert a[^3] == 2
    doAssert a[^4] == 1

  block:
    proc first[T](x, y: T): T = x
    var b: array[3, array[2, float]] = [[1.0,2],
                                        [3.0,4],
                                        [8.0,9]]

    doAssert b[^1][^1] == 9.0
    doAssert ( b[^2] ).first( b[^1] )[^1] == 4.0

    b[^1] = [8.8, 8.9]
    doAssert b[^1] == [8.8, 8.9]

  block:

    var c: seq[(int, int)] = @[(1,2), (3,4)]
    c[^1][1] = 5
    doAssert c == @[(1, 2), (3, 5)]

    proc deref(x: ptr int): int = x[]
    doAssert deref(addr c[^1][0]) == 3

  block:
    proc fromOpenArray(x: openArray[int]): int =
      return x[^2]

    doAssert fromOpenArray([1, 2, 3]) == 2

  block:
    proc mutOpenarray(x: var openArray[string], val: string) =
      x[^2] = val

    const value = "new value"
    var z = @["a", "b", "c"]

    mutOpenarray(z , value)
    doAssert z == @["a" , value , "c" ]

  block issue_6675:
    # Wrong indices in arrays not starting with 0
    var y: array[1..5, int] = [1,2,3,4,5]
    y[3..5] = [1, 2, 3]
    doAssert y[3..5] == @[1, 2, 3]

  block:
    var d: array['a'..'c', string] = ["a", "b", "c"]
    doAssert d[^1] == "c"




import strutils, sequtils, typetraits, os

type
  MetadataArray* = object
    data*: array[8, int]
    len*: int

# Commenting the converter removes the error "lib/system.nim(3536, 3) Error: for a 'var' type a variable needs to be passed"
converter toMetadataArray*(se: varargs[int]): MetadataArray {.inline.} =
  result.len = se.len
  for i in 0..<se.len:
    result.data[i] = se[i]


block troofregression:
  when NimVersion >= "0.17.3":
    type Index = int or BackwardsIndex
    template `^^`(s, i: untyped): untyped =
      when i is BackwardsIndex:
        s.len - int(i)
      else: i
  else:
    type Index = int
    template `^^`(s, i: untyped): untyped =
      i

  ## With Nim devel from the start of the week (~Oct30) I managed to trigger "lib/system.nim(3536, 4) Error: expression has no address"
  ## but I can't anymore after updating Nim (Nov5)
  ## Now commenting this plain compiles and removes the error "lib/system.nim(3536, 3) Error: for a 'var' type a variable needs to be passed"
  proc `[]`(a: var MetadataArray, idx: Index): var int {.inline.} =
    a.data[a ^^ idx]


  ##############################
  ### Completely unrelated lib that triggers the issue

  type
    MySeq[T] = ref object
      data: seq[T]

  proc test[T](sx: MySeq[T]): T =
    # Removing the backward index removes the error "lib/system.nim(3536, 3) Error: for a 'var' type a variable needs to be passed"
    return sx.data[^1] # error here

  let s = MySeq[int](data: @[1, 2, 3])
  doAssert s.test() == 3


  # bug #6989

  type Dist = distinct int

  proc mypred[T: Ordinal](x: T): T = T(int(x)-1)
  proc cons(x: int): Dist = Dist(x)

  var d: Dist

  template `^+`(s, i: untyped): untyped =
    (when i is BackwardsIndex: s.len - int(i) else: int(i))

  proc `...`[T, U](a: T, b: U): HSlice[T, U] =
    result.a = a
    result.b = b

  proc `...`[T](b: T): HSlice[int, T] =
    result.b = b

  template `...<`(a, b: untyped): untyped =
    ## a shortcut for 'a..pred(b)'.
    a ... pred(b)

  doAssert typeof(4 ...< 1).name == "HSlice[system.int, system.int]"
  doAssert typeof(4 ...< ^1).name == "HSlice[system.int, system.BackwardsIndex]"
  doAssert typeof(4 ... pred(^1)).name == "HSlice[system.int, system.BackwardsIndex]"
  doAssert typeof(4 ... mypred(8)).name == "HSlice[system.int, system.int]"
  doAssert typeof(4 ... mypred(^1)).name == "HSlice[system.int, system.BackwardsIndex]"

  var rot = 8

  proc bug(s: string): string =
    result = s
    result = result[result.len - rot .. ^1] & "__" & result[0 ..< ^rot]

  const testStr = "abcdefgasfsgdfgsgdfggsdfasdfsafewfkljdsfajsdflfdjkl"

  doAssert ( bug(testStr) ==
    "dflfdjkl__abcdefgasfsgdfgsgdfggsdfasdfsafewfkljdsfajs")

  doAssert ( testStr[testStr.len - 8 .. testStr.len - 1] & "__" &
     testStr[0 .. testStr.len - pred(rot)] ==
    "dflfdjkl__abcdefgasfsgdfgsgdfggsdfasdfsafewfkljdsfajsdf")

  # js can't `readFile` so we read it into memory and compile it in

  const instructions = staticRead( parentDir(currentSourcePath) / "troofregression2.txt" ).split(',')

  proc dance(dancers: string): string =
    result = dancers
    for instr in instructions:
      let rem = instr[1 .. instr.high]
      case instr[0]
      of 's':
        let rot = rem.parseInt
        result = result[result.len - rot .. ^1] & result[0 ..< ^rot]
      of 'x':
        let
          x = rem.split('/')
          a = x[0].parseInt
          b = x[1].parseInt
        swap(result[a], result[b])
      of 'p':
        let
          a = result.find(rem[0])
          b = result.find(rem[^1])
        result[a] = rem[^1]
        result[b] = rem[0]
      else: discard

  proc longDance(dancers: string, iterations = 1_000_000_000): string =
    var
      dancers = dancers
      seen = @[dancers]
    for i in 1 .. iterations:
      dancers = dancers.dance()
      if dancers in seen:
        return seen[iterations mod i]
      seen.add(dancers)



  var programs = "abcdefghijklmnop"
  doAssert dance(programs) == "kgdchlfniambejop"
  doAssert longDance(programs) == "fjpmholcibdgeakn"


when not defined(js):
  block tunchecked:
    {.boundchecks: on.}
    type Unchecked = UncheckedArray[char]

    var x = cast[ptr Unchecked](alloc(100))
    x[5] = 'x'



import macros
block issue_7818:
   #inconsistent internal representation of generic objects array construction
  type
    Vehicle[T] = object of RootObj
      tire: T
    Car[T] = object of Vehicle[T]
    Bike[T] = object of Vehicle[T]

  macro peek(n: typed): untyped =
    let val = getTypeImpl(n).treeRepr
    newLit(val)

  block test_t7818:
    var v = Vehicle[int](tire: 3)
    var c = Car[int](tire: 4)
    var b = Bike[int](tire: 2)

    let y = peek([c, b, v])
    let z = peek([v, c, b])
    doAssert(y == z)

  block test_t7906_1:
    proc init(x: typedesc, y: int): ref x =
      result = new(ref x)
      result.tire = y

    var v = init(Vehicle[int], 3)
    var c = init(Car[int], 4)
    var b = init(Bike[int], 2)

    let y = peek([c, b, v])
    let z = peek([v, c, b])
    doAssert(y == z)

  block test_t7906_2:
    var v = Vehicle[int](tire: 3)
    var c = Car[int](tire: 4)
    var b = Bike[int](tire: 2)

    let y = peek([c.addr, b.addr, v.addr])
    let z = peek([v.addr, c.addr, b.addr])
    doAssert(y == z)

  block test_t7906_3:
    type
      Animal[T] = object of RootObj
        hair: T
      Mammal[T] = object of Animal[T]
      Monkey[T] = object of Mammal[T]

    var v = Animal[int](hair: 3)
    var c = Mammal[int](hair: 4)
    var b = Monkey[int](hair: 2)

    let z = peek([c.addr, b.addr, v.addr])
    let y = peek([v.addr, c.addr, b.addr])
    doAssert(y == z)

  type
    Fruit[T] = ref object of RootObj
      color: T
    Apple[T] = ref object of Fruit[T]
    Banana[T] = ref object of Fruit[T]

  proc testArray[T](x: array[3, Fruit[T]]): string =
    result = ""
    for c in x:
      result.add $c.color

  proc testOpenArray[T](x: openArray[Fruit[T]]): string =
    result = ""
    for c in x:
      result.add $c.color

  block test_t7906_4:
    var v = Fruit[int](color: 3)
    var c = Apple[int](color: 4)
    var b = Banana[int](color: 2)

    let y = peek([c, b, v])
    let z = peek([v, c, b])
    doAssert(y == z)

  block test_t7906_5:
    var a = Fruit[int](color: 1)
    var b = Apple[int](color: 2)
    var c = Banana[int](color: 3)

    doAssert(testArray([a, b, c]) == "123")
    doAssert(testArray([b, c, a]) == "231")

    doAssert(testOpenArray([a, b, c]) == "123")
    doAssert(testOpenArray([b, c, a]) == "231")

    doAssert(testOpenArray(@[a, b, c]) == "123")
    doAssert(testOpenArray(@[b, c, a]) == "231")

  proc testArray[T](x: array[3, ptr Vehicle[T]]): string =
    result = ""
    for c in x:
      result.add $c.tire

  proc testOpenArray[T](x: openArray[ptr Vehicle[T]]): string =
    result = ""
    for c in x:
      result.add $c.tire

  block test_t7906_6:
    var u = Vehicle[int](tire: 1)
    var v = Bike[int](tire: 2)
    var w = Car[int](tire: 3)

    doAssert(testArray([u.addr, v.addr, w.addr]) == "123")
    doAssert(testArray([w.addr, u.addr, v.addr]) == "312")

    doAssert(testOpenArray([u.addr, v.addr, w.addr]) == "123")
    doAssert(testOpenArray([w.addr, u.addr, v.addr]) == "312")

    doAssert(testOpenArray(@[u.addr, v.addr, w.addr]) == "123")
    doAssert(testOpenArray(@[w.addr, u.addr, v.addr]) == "312")

block trelaxedindextyp:
  # any integral type is allowed as index
  proc foo(x: ptr UncheckedArray[int]; idx: uint64): int = x[idx]
  proc foo(x: seq[int]; idx: uint64): int =  x[idx]
  proc foo(x: string|cstring; idx: uint64): int = x[idx]
  proc foo(x: openArray[int]; idx: uint64): int = x[idx]

block issue_3899:
  #  Wrong bounds check using template [] to access array in a const object
  type O = object
    a: array[1..2,float]
  template `[]`(x: O, i: int): float =
    x.a[i]

  const c = O(a: [1.0,2.0])
  doAssert c[2] == 2.0

block arrayLiterals:
  type ABC = enum A, B, C
  template Idx[IdxT, ElemT](arr: array[IdxT, ElemT]): untyped = IdxT
  doAssert [A: 0, B: 1].Idx is range[A..B]
  doAssert [A: 0, 1, 3].Idx is ABC
  doAssert [1: 2][1] == 2
  doAssert [-1'i8: 2][-1] == 2
  doAssert [-1'i8: 2, 3, 4, 5].Idx is range[-1'i8..2'i8]



block issue_8316:
  #BUG: "varargs[string, $]" calls $ n^2 times instead of n times (n=len(varargs))
  var cnt : int = 0
  proc myAppend[T](a:T):string=
    cnt += 1
    return $a

  template append2(args: varargs[string, myAppend]): string =
    var ret:string
    for a in args:
      ret.add(a)
    ret

  doAssert append2("1", "2", "3") == "123"
  doAssert cnt == 3

block issue_12466:
  # Crash in sameTree() with uint literals
  var a: array[288, uint16]
  for i in 0'u16 ..< 144'u16:
    a[0'u16 + i] = i
  for i in 0'u16 ..< 8'u16:
    a[0'u16 + i] = i

block issue_17705:
  #  Fix array's high & low return type for empty arrays
  var a = array[0, int].low
  a = int(a)
  var b = array[0, int].high
  b = int(b)

block issue_18643:
  # Accessing an empty array sometimes works
  let a: array[0, int] = []
  var caught = false
  let b = 9999999
  try:
    discard a[b]
    doAssert false,  "IndexDefect not caught!"
  except IndexDefect:
    doAssert true
