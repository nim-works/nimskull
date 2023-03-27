discard """
  outputsub: "true"
  disabled: "32bit"
"""

type
  TFoo* = object
    id: int
    fn: proc() {.closure.}
var foo_counter = 0
var alive_foos = newseq[int](0)

proc `=destroy`(some: var TFoo) =
  alive_foos.del alive_foos.find(some.id)
  `=destroy`(some.fn)

proc newFoo*(): ref TFoo =
  new result
  result.id = foo_counter
  alive_foos.add result.id
  inc foo_counter

for i in 0 ..< 10:
  discard newFoo()

for i in 0 ..< 10:
  let f = newFoo()
  f.fn = proc =
    echo f.id

GC_fullcollect()
echo alive_foos.len <= 3
