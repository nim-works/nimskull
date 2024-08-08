discard """
  description: '''
    Hook routines are not allowed to raise exception. If they're inferred to
    raise, a compile-time error is reported.
  '''
  action: reject
  matrix: "--errorMax:5"
"""

type Type = object

proc `=copy`(a: var Type, b: Type) =
  raise (ref CatchableError)() #[tt.Error
  ^ a hook routine is not allowed to raise. (ref CatchableError)]#

proc `=sink`(a: var Type, b: Type) =
  raise (ref CatchableError)() #[tt.Error
  ^ a hook routine is not allowed to raise. (ref CatchableError)]#

proc `=destroy`(a: var Type) =
  raise (ref CatchableError)() #[tt.Error
  ^ a hook routine is not allowed to raise. (ref CatchableError)]#

proc `=trace`(a: var Type, env: pointer) =
  raise (ref CatchableError)() #[tt.Error
  ^ a hook routine is not allowed to raise. (ref CatchableError)]#

proc `=deepCopy`(a: ref Type): ref Type =
  raise (ref CatchableError)() #[tt.Error
  ^ a hook routine is not allowed to raise. (ref CatchableError)]#
