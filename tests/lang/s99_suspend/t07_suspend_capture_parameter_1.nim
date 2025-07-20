discard """
  description: '''
    Non-owning parameters and view parameters are not copied/moved into the
    suspend block upon use.
  '''
  output: ""
"""

type Object = object

proc `=copy`(a: var Object, b: Object) =
  echo "copied"

proc use(x: Object) = discard

proc test(x: Object) =
  suspend void, cont:
    use(x)

test(Object())
