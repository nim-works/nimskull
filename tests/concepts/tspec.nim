discard """
  output: '''4
0
4
4
1
2
3
yes int
string int'''
  joinable: false
"""

import hashes

type
  Comparable = concept a, b
    cmp(a, b) is int

  ToStringable = concept a
    $a is string

  Hashable = concept x, y  ## the most basic of identity assumptions
    hash(x) is int
    x == y is bool

  Swapable = concept x, y
    swap(x, y)


proc h(x: Hashable) =
  echo x

h(4)

when true:
  proc compare(a: Comparable) =
    echo cmp(a, a)

  compare(4)

proc dollar(x: ToStringable) =
  echo x

when true:
  dollar 4
  dollar "4"

#type D = distinct int

#dollar D(4)

when true:
  type
    Iterable[Ix] = concept c
      for i in items(c):
        i is Ix

  proc g[Tu](it: Iterable[Tu]) =
    for x in it:
      echo x

  g(@[1, 2, 3])

proc hs(x: Swapable) =
  var y = x
  swap y, y

hs(4)

type
  Indexable[T] = concept a # has a T, a collection
    a[int] is T # we need to describe how to infer 'T'
    # and then we can use the 'T' and it must match:
    a[int] = T
    len(a) is int

proc indexOf[T](a: Indexable[T]; value: T) =
  echo "yes ", T

block:
  var x = @[1, 2, 3]
  indexOf(x, 4)

import tables, typetraits

type
  Dict[K, V] = concept s
    s[K] is V
    s[K] = V

proc d[K2, V2](x: Dict[K2, V2]) =
  echo K2, " ", V2

var x = initTable[string, int]()
d(x)


type Monoid = concept x, y
  x + y is type(x)
  z(typedesc[type(x)]) is type(x)

proc z(x: typedesc[int]): int = 0

doAssert int is Monoid

