discard """
  matrix: "--errorMax:2"
  action: reject
"""

proc a(x: openArray[int]) {.coroutine.} = #[tt.Error
          ^ invalid type: 'openArray[int]']#
  discard

proc b(x: var int) {.coroutine.} = #[tt.Error
          ^ invalid type: 'var int']#
  discard
