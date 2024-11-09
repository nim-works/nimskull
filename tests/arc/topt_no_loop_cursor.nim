discard """
  description: '''
    Ensure that outer variables don't borrow from locals within loops, when not
    safe
  '''
  matrix: "--showir:mir_in:test --hints:off"
  action: compile
  nimoutFull: true
  nimout: '''-- MIR: test
scope:
  def x: Object
  scope:
    while true:
      scope:
        scope:
          def _3: bool = not(arg cond)
          if _3:
            scope:
              goto [L2]
        def y: Object = ()
        def_cursor _5: Object = x
        use(arg _5) -> [L3]
        x = sink y
        destroy y
        goto [L4]
        finally (L3):
          destroy y
          continue [L5]
        L4:
  L2:
  destroy x
  goto [L6]
  finally (L5):
    destroy x
    continue [Resume]
  L6:

-- end
'''
"""

type Object = object

# make Object a type that's eligible for cursor inference
proc `=destroy`(x: var Object) =
  discard

proc use(x: Object) =
  discard

proc test(cond: bool) =
  var x: Object
  while cond:
    var y = Object()
    use x
    # if `x` were a cursor, the above usage would observe a stale value,
    # as the value assigned below went out of scope already
    x = y

test(false)
