discard """
  output: '''0
1
2
3
4
5
6
7
8
9
10
5 5
7 7
9 9
0
0
0
0
1
2
70
0
'''
"""

block closure_iterator_is_closure:
  iterator iter() {.closure.} =
    discard

  # always returns true, even if the iterator doesn't capture anything
  doAssert iter is "closure"

when true:
  proc main() =
    let
      lo=0
      hi=10

    iterator itA(): int =
      for x in lo..hi:
        yield x

    for x in itA():
      echo x

    var y: int

    iterator itB(): int =
      while y <= hi:
        yield y
        inc y

    y = 5
    for x in itB():
      echo x, " ", y
      inc y

  main()


iterator infinite(): int {.closure.} =
  var i = 0
  while true:
    yield i
    inc i

iterator take[T](it: iterator (): T, numToTake: int): T {.closure.} =
  var i = 0
  for x in it():
    if i >= numToTake:
      break
    yield x
    inc i

# gives wrong reasult (3 times 0)
for x in infinite.take(3):
  echo x

# does what we want
let inf = infinite
for x in inf.take(3):
  echo x

# bug #3583
proc foo(f: (iterator(): int)) =
  for i in f(): echo i

let fIt = iterator(): int = yield 70
foo fIt

# bug #5321

proc lineIter*(filename: string): iterator(): string =
  result = iterator(): string {.closure.} =
    for line in lines(filename):
      yield line

proc unused =
  var count = 0
  let iter = lineIter("temp10.nim")
  for line in iter():
    count += 1

iterator lineIter2*(filename: string): string {.closure.} =
  var f = open(filename, bufSize=8000)
  defer: close(f)   # <-- commenting defer "solves" the problem
  var res = newStringOfCap(80)
  while f.readLine(res): yield res

proc unusedB =
  var count = 0
  for line in lineIter2("temp10.nim"):
    count += 1


# bug #13815
var love = iterator: int {.closure.} =
  yield cast[type(
    block:
      var a = 0
      yield a
      a)](0)

for i in love():
  echo i
