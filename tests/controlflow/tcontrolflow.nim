discard """
  joinable: false
  output: '''
10
true true
true false
false true
false false
i == 2
'''
"""


block tbreak:
  var
    x = false
    run = true

  while run:
    run = false
    block myblock:
      if true:
        break
      echo "leaving myblock"
    x = true
  doAssert(x)

  # bug #1418
  iterator foo: int =
    for x in 0 .. 9:
      for y in [10,20,30,40,50,60,70,80,90]:
        yield x + y

  for p in foo():
    echo p
    break

  iterator permutations: int =
    yield 10

  for p in permutations():
    break

  # regression:
  proc main =
    for x in [true, false]:
      for y in [true, false]:
        echo x, " ", y

  main()



block tcontinue:
  var i = 0
  while i < 400:

    if i == 10: break
    elif i == 3:
      inc i
      continue
    inc i

  var f = "failure"
  var j = 0
  while j < 300:
    for x in 0..34:
      if j < 300: continue
      if x == 10:
        echo "failure: should never happen"
        break
    f = "came here"
    break

  if i == 10:
    doAssert f == "came here"
  else:
    echo "failure"



block tnestif:
  var
      x, y: int
  x = 2
  if x == 0:
      if y == 0:
          echo x
      else:
          echo y
  elif x == 1:
      echo "i == 1"
  elif x == 2:
      echo "i == 2"
  else:
      echo "looks like Python"
  #OUT i == 2
