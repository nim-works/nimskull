discard """
  description: '''
    Ensure that the destructor call for the result variable is optimized away
    when possible
  '''
  matrix: "--expandArc:test --hints:off"
  nimoutfull: true
  nimout: '''--expandArc: test
scope:
  scope:
    if x:
      scope:
        doRaise() -> [L1, Resume]
  def _2: Object = ()
  result := move _2
goto [L2]
finally (L1):
  continue {}
L2:

-- end of expandArc ------------------------
'''
"""

type Object = object

proc `=destroy`(x: var Object) =
  discard

proc doRaise() =
  raise CatchableError.newException("")

proc test(x: bool): Object {.exportc.} =
  if x:
    # raise with a separate procedure so that the ``--expandArc`` output is
    # shorter
    doRaise()

  result = Object()
