discard """
  description: "Locals that must be saved have to be copyable."
  action: reject
"""

type Object = object

proc `=copy`(x: var Object, y: Object) {.error.}

proc test() =
  var x = Object()
  suspend void, cont:
    discard

test()
