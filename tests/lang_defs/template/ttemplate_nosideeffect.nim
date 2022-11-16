discard """
  output: '''
start
side effect!
end
'''
description: '''
  . From https://github.com/nim-lang/Nim/issues/6217
    no side effect pragma problem
  . https://github.com/nim-lang/Nim/pull/19410
    Fix term rewriting with sideeffect
'''
"""
template optMul{`*`(a, 2)}(a: int{noSideEffect}): int = a+a

proc f(): int =
  echo "side effect!"
  result = 55

echo "start"
doAssert f() * 2 == 110
echo "end"

