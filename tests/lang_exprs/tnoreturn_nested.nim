discard """
  description: """
    Ensure nested noreturn statements are considererd in noreturn analysis
  """
"""

block nested_in_if:
  let x =
    if true:
      0
    else:
      if true:
        raise newException(CatchableError, "error")
      else:
        raise newException(CatchableError, "another error")

  doAssert x is int

block nested_in_try:
  let x =
    if true:
      0
    else:
      try:
        raise newException(CatchableError, "error")
      finally:
        discard

  doAssert x is int

block nested_in_case:
  let x =
    if true:
      0
    else:
      let s = false
      case s
      of true:
        raise newException(CatchableError, "error")
      of false:
        raise newException(CatchableError, "another error")

  doAssert x is int

block nested_in_block:
  let x =
    if true:
      0
    else:
      block:
        raise newException(CatchableError, "error")

  doAssert x is int