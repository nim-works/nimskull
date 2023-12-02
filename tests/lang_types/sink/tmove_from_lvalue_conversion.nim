discard """
  targets: "c js vm"
  description: '''
    Destrutively moving from an up- or down converted `ref` value passed as a
    `sink` parameter must work
  '''
"""

type
  A = object of RootObj
    val: int
  B = object of A
  C = object of B

var instance = (ref C)(val: 1)

template test(target: typedesc, explicit: static[bool]) =
  block:
    proc conv(x: sink(ref B)) =
      when explicit:
        let y = move(target(x))
      else:
        let y = target(x)

      # force a destructive move by reassigning `x`
      x = new(B)

      # test the value after moving the ref to make sure the move really
      # worked at run-time
      doAssert y.val == 1

    conv(instance)

# down conversion
test(ref A, true)
test(ref A, false)

# up conversion
test(ref C, true)
test(ref C, false)