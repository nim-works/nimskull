discard """
  description: "Converters cannot be coroutines"
  action: reject
"""

# XXX: maybe to restrictive, there's nothing preventing converters from being
#      coroutines, even though there's likely little use of them being one

converter conv(x: int): float {.coroutine.} =
  discard
