discard """
  errormsg: "type mismatch: got <array[0..2, float], array[0..1, float]>"
  labels: "generic proc resolution range"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/202
      Bunch of issues. Generics related
    . Generics fail to match exactly on range specs of arrays
'''
"""

proc `+`*[R, T] (v1, v2: array[R, T]): array[R, T] =
  for i in low(v1)..high(v1):
    result[i] = v1[i] + v2[i]

var
  v1: array[0..2, float] = [3.0, 1.2, 3.0]
  v2: array[0..1, float] = [2.0, 1.0]
  v3 = v1 + v2

