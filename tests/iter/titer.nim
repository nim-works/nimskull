discard """
output: '''
testtesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttesttest2!test3?
hi
what's
your
name
hi
what's
your
name
'''
"""

# Test the new iterators

iterator xrange(fromm, to: int, step = 1): int =
  var a = fromm
  while a <= to:
    yield a
    inc(a, step)

iterator interval[T](a, b: T): T =
  var x = a
  while x <= b:
    yield x
    inc(x)

#
#iterator lines(filename: string): (line: string) =
#  var
#    f: tTextfile
#    shouldClose = open(f, filename)
#  if shouldClose:
#    setSpace(line, 256)
#    while readTextLine(f, line):
#      yield line
#  finally:
#    if shouldClose: close(f)
#

var thingy = ""
for i in xrange(0, 5):
  for k in xrange(1, 7):
    thingy.add "test"

for j in interval(45, 45):
  thingy.add "test2!"
  thingy.add "test3?"

echo thingy

for x in items(["hi", "what's", "your", "name"]):
  echo(x)

const
  stringArray = ["hi", "what's", "your", "name"]

for i in 0..len(stringArray)-1:
  echo(stringArray[i])

# bug #15360

type Rule[T] = (int, T)

var t: seq[Rule[int]]
for (c, t) in t:
  discard



import std/sugar

# bug #14165
block nim_bug_14165:
  when not defined(js):
    # xxx: this has the usual js codegen issues, not worth splitting this out
    iterator log_nodups_hamming(): int {.inline.} =
      let lb3 = 1
      let lb4 = 123
      proc mul3(): int = lb3 + lb4
      yield mul3()

    for h in log_nodups_hamming():
      break
    for h in log_nodups_hamming():
      break
    for h in log_nodups_hamming():
      break

# bug #18536
block nim_bug_18536:
  when not defined(js):
    # xxx: this has a weird js/vm issue, not splitting out because tired
    iterator envPairs(): int =
      var foo: seq[int]
      proc fun() =
        foo = @[]
      fun()
      yield 3

    proc main() =
      for a in envPairs():
        discard
      for a in envPairs():
        discard
    static: main()
    main()

# bug #6269
block nim_bug_6269:
  when not defined(js):
    # xxx: this has the usual js codegen issues, not worth splitting this out
    iterator makeFn(outer_val: int): proc(a: int): int =
      for i in 0..1:
        yield proc(a:int): int =
          return a + i.int

    let v1 = 42

    let res = collect:
      for fn1 in makeFn(v1):
        let v2 = fn1(v1)
        for fn2 in makeFn(v2):
          fn2(v2)

    doAssert res == @[42, 43, 43, 44]
