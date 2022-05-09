discard """
output: '''
M=1000
D=500
C=100
L=50
X=10
V=5
I=1
'''
"""

const romanNumbers = [
    ("M", 1000), ("D", 500), ("C", 100),
    ("L", 50), ("X", 10), ("V", 5), ("I", 1) ]

var c = 0
for key, val in items(romanNumbers):
  inc(c)
  echo key & "=" & $val
#echo""

proc PrintBiTuple(t: tuple[k: string, v: int]): int =
  echo(t.k & "=" & $t.v & ", ")
  return 0
