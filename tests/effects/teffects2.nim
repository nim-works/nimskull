discard """
  errormsg: "can raise an unlisted exception: ref IOError"
  line: 19
"""
{.push warningAsError[Effect]: on.}
type
  TObj = object {.pure, inheritable.}
  TObjB = object of TObj
    a, b, c: string

  EIO2 = ref object of IOError

proc forw: int {.raises: [].}

proc lier(): int {.raises: [IOError].} =
  echo "arg"

proc forw: int =
  raise newException(IOError, "arg")
{.pop.}
