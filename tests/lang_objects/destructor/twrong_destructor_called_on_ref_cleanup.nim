discard """
  description: '''
    Regression test for certain ref types calling the wrong destructor
  '''
  output: '''proc (x: int){.closure.}
proc (x: sink int){.closure.}
'''
"""

type Object[T] = object
  x: T

proc `=destroy`[T](x: var Object[T]) =
  echo T

discard (ref Object[proc(x: sink int)])()
discard (ref Object[proc(x: int)])()
