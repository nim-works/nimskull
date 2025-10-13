type Object* = object
  val*: int

proc `=destroy`(x: var Object) =
  if x.val != 0:
    echo x.val

let global = Object(val: 7)
var tv1 {.threadvar.}: Object
tv1 = Object(val: 8)

proc init*() =
  tv1 = Object(val: 3)
  var tv2 {.threadvar.}: Object
  tv2 = Object(val: 4)
