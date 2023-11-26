discard """
  targets: "c js vm"
  description: '''
    Ensure that move optimizer doesn't turn assignments with cursor fields on
    the left into moves (cursors cannot be moved into owning locations)
  '''
"""

type
  Destructible = object
    valid: bool
    val: int

var destroyed = false

proc `=destroy`(x: var Destructible) =
  if x.valid and x.val == 1:
    doAssert not destroyed, "value is destroyed twice!"
    destroyed = true

proc `=copy`(x: var Destructible, y: Destructible) =
  x.valid = y.valid
  x.val = y.val
  if x.valid:
    # increment the counter so that non-copies can be detected
    inc x.val

func mut(x: var Destructible) {.noinline.} =
  discard

type
  Object = object
    x {.cursor.}: Destructible

func main() =
  var v = Destructible(valid: true, val: 1)

  var x = Object()
  x.x = v # this only creates a shallow copy of `v`

  var y = x.x

  # reassign the cursor field, which would allow the above usage of `x.x`
  # to be turned into a move (if it weren't a cursor). `x.x` must **not**
  # be moved out of, as it doesn't store a value that can be destroyed
  x.x = Destructible(valid: false)
  # mutate `y` so that it cannot be turned into a cursor automatically
  mut y

main()
doAssert destroyed, "value wasn't destroyed at all"