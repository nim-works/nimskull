discard """
  output: '''2
4
6
4
8
12
'''
    knownIssue: "lambda lifting support for iterToProc plugin; lambda lifting and iterators are likley to go through a bit spec definition and this will either get folded in and its a good signal to know whether we fixed something by happenstance or not."
"""

# Will eventually fix it...

iterator map[T, U](s: iterator:T{.inline.}, f: proc(x: T): U): U =
  for e in s: yield f(e)

template toSeq(s: untyped): untyped =
  var res = newSeq[type(s)](0)
  for e in s: res.add(e)
  res

var s1 = @[1, 2, 3]
for x in map(s1.items, proc (a:int): int = a*2):
  echo x

var s2 = toSeq(map(s1.items, proc (a:int): int = a*4))
for x in s2:
  echo x

