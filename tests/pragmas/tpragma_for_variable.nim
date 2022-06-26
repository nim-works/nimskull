discard """
  cmd: "nim check --hint:processing:off $file"
  errormsg: "3 is not two"
  nimout: '''tpragma_for_variable.nim(19, 9) Error: cannot attach a custom pragma to 'a'
tpragma_for_variable.nim(35, 15) template/generic instantiation of `onlyTwo` from here
tpragma_for_variable.nim(31, 12) Error: 3 is not two
'''
  description: '''
    . From https://github.com/nim-lang/Nim/issues/8741
      Unknown Pragma ignored when applied to for loop variable
    . https://github.com/LemonBoy/Nim/commit/972008a0d758f66bd99f09bf4f3fbfbe473c20bb
      Validate pragmas attached to for variables
  '''
"""

for a {.gensym, inject.} in @[1,2,3]:
  discard

for a {.foobar.} in @[1,2,3]:
  discard

type Foo[N: static[int]] = distinct int

proc isTwo(n: int): bool =
  n == 2

proc onlyTwo[N: static[int]](a: Foo[N]): int =
  when isTwo(N):
    int(a)
  else:
    {.error: $(N) & " is not two".}

when isMainModule:
  let foo: Foo[3] = Foo[3](5)
  echo onlyTwo(foo)

