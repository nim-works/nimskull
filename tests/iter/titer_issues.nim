discard """
  output: '''
9002
9004
9006
9008
9010
9012
9014
9016
9018
@[1, 2]
@[1, 2, 3]
'''
"""


import sequtils, strutils


block t3221_complex:
  iterator permutations[T](ys: openarray[T]): seq[T] =
    var
      d = 1
      c = newSeq[int](ys.len)
      xs = newSeq[T](ys.len)
    for i, y in ys: xs[i] = y
    yield xs
    block outer:
      while true:
        while d > 1:
          dec d
          c[d] = 0
        while c[d] >= d:
          inc d
          if d >= ys.len: break outer
        let i = if (d and 1) == 1: c[d] else: 0
        swap xs[i], xs[d]
        yield xs
        inc c[d]

  proc dig_vectors(): void =
    var v_nums: seq[int]
    v_nums = newSeq[int](1)
    for perm in permutations(toSeq(0 .. 1)):
      v_nums[0] = 1

  dig_vectors()



block:
  # bug #13739
  iterator myIter(arg: openarray[int]): int =
    var tmp = 0
    let len = arg.len
    while tmp < len:
      yield arg[tmp] * 2
      inc tmp

  proc someProc() =
    var data = [4501,4502,4503,4504,4505,4506,4507,4508,4509]
    # StmtListExpr should not get special treatment.
    for x in myIter((discard;data)):
      echo x

  someProc()

block:
  # bug #12576
  iterator ff(sq: varargs[seq[int]]): int =
    for x in sq:
      echo x

  for x in ff(@[1, 2], @[1, 2, 3]):
    echo x
