discard """
  output: '''foo 0
bar 0
baz'''
  targets: "c js vm"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/1641
      Incorrect closure code generation
    . The following program generates code that doesn't compile.
      Note that either replacing do(x): with do(x: int) or
      putting the code inside a function will prevent the error.
  '''
"""

proc baz() =
  echo "baz"

proc bar(x: int, p: proc()) =
  echo "bar ", x
  p()

proc foo(x: int, p: proc(x: int)) =
  echo "foo ", x
  p(x)

let x = 0
x.foo do(x: int): x.bar do(): baz()