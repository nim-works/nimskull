discard """
  description: '''
  . From https://github.com/nim-lang/Nim/issues/8012
    Recursion using concepts crashes compiler
  . It works since Nim v1.4
'''
"""

type
  MyTypeCon = concept c
    c.counter is int
  MyType = object
    counter: int

proc foo(conc: var MyTypeCon) =
  conc.counter.inc
  if conc.counter < 5:
    foo(conc)

var x: MyType

x.foo
discard x.repr