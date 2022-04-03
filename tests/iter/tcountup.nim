discard """
  output: '''
0
1
2
3
4
5
6
7
8
9
0.0
'''
"""

# Test new countup

for i in 0 ..< 10'i64:
  echo i

# 11099

var
  x: uint32
  y: float

for i in 0 ..< x:
  if i == 1: echo i
  y += 1

echo y
