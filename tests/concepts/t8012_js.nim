discard """
  target: js
  knownIssue: "emits invalid code in js codegen; `conc.counter` is undefined"
  description: "broken js codegen for concepts, combine with t8012 once fixed"
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
