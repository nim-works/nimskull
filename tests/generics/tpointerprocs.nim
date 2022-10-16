discard """
cmd: "nim check $options --hints:off $file"
action: "reject"
nimout:'''
tpointerprocs.nim(14, 11) Error: 'foo' doesn't have a concrete type, due to unspecified generic parameters.
tpointerprocs.nim(15, 11) Error: identifier 'bar' found but it has errors, see: tpointerprocs.nim(14, 5)
tpointerprocs.nim(26, 11) Error: cannot instantiate: 'foo[int]'; got 1 typeof(s) but expected 2
tpointerprocs.nim(27, 11) Error: identifier 'bar' found but it has errors, see: tpointerprocs.nim(26, 5)
'''
"""
block:
  proc foo(x: int | float): float = result = 1.0
  let
    bar = foo
    baz = bar

block:
  proc foo(x: int | float): float = result = 1.0
  let
    bar = foo[int]
    baz = bar

block:
  proc foo(x: int | float, y: int or string): float = result = 1.0
  let
    bar = foo[int]
    baz = bar
