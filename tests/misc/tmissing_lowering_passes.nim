discard """
  description: '''
    Regression test for a compiler bug where some lowering passes were not
    applied to initializer expressions of ``.global`` variables
  '''
  target: native
"""

proc p() =
  var x {.global.} = block:
    var a, b: int
    swap(a, b)
    # no lowering pass took place for initializer expression, causing an
    # internal compiler error due to the unlowered ``swap`` magic
    a

p()