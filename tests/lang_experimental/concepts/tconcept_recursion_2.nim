discard """
  output: '''10
9
8
7
6
5
4
3
2
1
go!
'''
  description: '''
  . From https://github.com/nim-lang/Nim/issues/8558
    Compiler stack overflow using recursive function with concepts
'''
"""

type Integral = concept x
  x == 0 is bool
  x - 1 is type(x)

proc countToZero(n: Integral) =
  if n == 0: echo "go!"
  else:
    echo n
    countToZero(n-1)

countToZero(10)