discard """
  targets: "native"
  output: '''
0
1
2
3
4
1
start
false
0
1
2
end
@[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 18, 20, 21, 24, 27, 30, 36, 40, 42]
1002
0
1
2
7
'''
"""

import sequtils, strutils

block t338:
  proc moo(): iterator (): int =
    iterator fooGen: int {.closure.} =
      while true:
        yield result
        result.inc
    return fooGen

  var foo = moo()

  for i in 0 .. 4:
    echo foo()



block t8041:
  iterator xy[T](a: T, b: set[T]): T =
    if a in b:
      yield a

  for a in xy(1'i8, {}):
    for b in xy(a, {}):
      echo a



block t3837_chained:
  iterator t1(): int {.closure.} =
    yield 1

  iterator t2(): int {.closure.} =
    for i in t1():
      yield i

  for i in t2():
    echo $i


  proc iter1(): (iterator: int) =
    let coll = [0,1,2]
    result = iterator: int {.closure.} =
      for i in coll:
        yield i

  proc iter2(it: (iterator: int)): (iterator: int)  =
    result = iterator: int {.closure.} =
      echo finished(it)
      for i in it():
        yield i

  echo "start"
  let myiter1 = iter1()
  let myiter2 = iter2(myiter1)
  for i in myiter2():
    echo i
  echo "end"

  type Iterable[T] = (iterator: T) | Slice[T]
    ## Everything that can be iterated over, iterators and slices so far.

  proc toIter[T](s: Slice[T]): iterator: T =
    ## Iterate over a slice.
    iterator it: T {.closure.} =
      for x in s.a..s.b:
        yield x
    return it

  proc toIter[T](i: iterator: T): iterator: T =
    ## Nop
    i

  iterator map[T,S](i: Iterable[T], f: proc(x: T): S): S =
    let i = toIter(i)
    for x in i():
      yield f(x)

  proc filter[T](i: Iterable[T], f: proc(x: T): bool): iterator: T =
    let i = toIter(i)
    iterator it: T {.closure.} =
      for x in i():
        if f(x):
          yield x
    result = it

  iterator filter[T](i: Iterable[T], f: proc(x: T): bool): T =
    let i = toIter(i)
    for x in i():
      if f(x):
        yield x

  var it = toSeq(filter(2..10, proc(x: int): bool = x mod 2 == 0))
  doAssert it == @[2, 4, 6, 8, 10]
  it = toSeq(map(filter(2..10, proc(x: int): bool = x mod 2 == 0), proc(x: int): int = x * 2))
  doAssert it == @[4, 8, 12, 16, 20]



block t3499_keepstate:
  proc slice[T](iter: iterator(): T {.closure.}, sl: auto): seq[T] =
    var res: seq[int64] = @[]
    var i = 0
    for n in iter():
      if i > sl.b:
        break
      if i >= sl.a:
        res.add(n)
      inc i
    res

  iterator harshad(): int64 {.closure.} =
    for n in 1 ..< int64.high:
      var sum = 0
      for ch in string($n):
        sum += parseInt("" & ch)
      if n mod sum == 0:
        yield n

  echo harshad.slice 0 ..< 20

  for n in harshad():
    if n > 1000:
      echo n
      break

  # bug #3499 last snippet fixed
  # bug #705  last snippet fixed



block t1725_nested:
  iterator factory(): int {.closure.} =
    iterator bar(): int {.closure.} =
      yield 0
      yield 1
      yield 2

    for x in bar(): yield x

  for x in factory():
    echo x



block t2023_objiter:
  type
    Obj = object
      iter: iterator (): int8 {.closure.}

  iterator test(): int8 {.closure.} =
    yield 7

  proc init():Obj=
    result.iter = test

  var o = init()
  echo(o.iter())
