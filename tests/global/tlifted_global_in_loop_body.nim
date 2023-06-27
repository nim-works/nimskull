discard """
  targets: "c js vm"
  description: '''
    Tests for globals defined inside for-loop bodies via the `.global` pragma
  '''
"""

block single_yield:
  # test with an inline iterator containing a single yield statement (so no
  # duplication of the body takes place)

  iterator iter(): int {.inline.} =
    yield 1

  proc test(): int =
    for _ in iter():
      var g {.global.} = 1
      inc g
      result = g

  doAssert test() == 2
  # verify that the global's value persists across calls:
  doAssert test() == 3

block multi_yield:
  # test with an inline iteratong that has multiple yield statements

  iterator iter(): int {.inline.} =
    yield 1
    yield 2
    yield 3

  proc test() =
    var a: ptr int = nil

    for i in iter():
      var g {.global.} = 1
      inc g

      # verfiy that the value persists:
      doAssert g == 1 + i, $g

      if a == nil:
        a = addr g # remember the address
      else:
        # verify that it really is the same location:
        doAssert a == addr g

  test()