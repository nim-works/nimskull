discard """
targets: "!js !vm"
description: '''
Enable or disable safety checks applicable to C and C++ targets
'''
"""

## The listed pragmas here can be used to override the code 
## generation options for a proc/method/converter.

block enable_bound_checking:
  {.push boundChecks:on.}
  proc impl() = 
    var gotDefect = false
    try:
      var a: array[10, int]
      let idx = 9
      assert cast[ptr array[9, int]](a.addr)[idx] == 0

    except IndexDefect:
      gotDefect = true

    doAssert gotDefect

  {.pop.}

  impl()

block disable_bound_checking:
  {.push boundChecks:off.}
  proc impl() = 

    var a: array[10, int]
    let idx = 9
    a[idx] = 10
    assert cast[ptr array[9, int]](a.addr)[idx] == 10


  {.pop.}

  impl()

block enable_overflow_checks:
  {.push overflowChecks:on.}
  proc impl() = 
    var gotDefect = false
    try:
      var value = high(int)
      inc value

    except OverflowDefect:
      gotDefect = true

    doAssert gotDefect

  {.pop.}

  impl()

block disable_overflow_checks:
  {.push overflowChecks:off.}
  proc impl() = 
    var value = high(int)
    inc value

  {.pop.}

  impl()