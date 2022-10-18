## This module implements various helper routines for idioms used throughout
## the compiler. Dependencies on other modules should be kept as minimal as
## possible

from std/private/miscdollars import toLocation

type
  IInfo = typeof(instantiationInfo())

func unreachableImpl(str: string, loc: IInfo) {.noinline, noreturn.} =
  var msg: string
  msg.toLocation(loc.filename, loc.line, loc.column + 1)
  msg.add:
    if str.len > 0: " unreachable: "
    else:           " unreachable"
  msg.add str
  raiseAssert(msg)

func unreachableImpl(e: enum, loc: IInfo) {.noinline, noreturn.} =
  ## A bit more efficient than ``unreachable($e)``, due to the stringication
  ## logic being located inside a separate procedure instead of at the
  ## callsite
  unreachableImpl($e, loc)

template unreachable*() =
  ## Use ``unreachable`` to mark a point in the program as unreachable. That
  ## is, execution must never reach said point and if it does, the event is
  ## treated as an unrecoverable fatal error.
  ## As the intent is clearer, it's preferred to use ``unreachable`` is
  ## over ``doAssert false``
  unreachableImpl("", instantiationInfo(-1))

template unreachable*(msg: string) =
  ## Similar to `#unreachable <#unreachable>`_, but reports an additional
  ## `msg` when reached
  unreachableImpl(msg, instantiationInfo(-1))

template unreachable*(e: enum) =
  ## More efficient than using ``unreachable($e)``
  unreachableImpl(e, instantiationInfo(-1))