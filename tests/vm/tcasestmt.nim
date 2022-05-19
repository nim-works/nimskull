discard """
  description: "Tests for case statement with selector of non-ordinal type"
  action: compile
"""

## XXX: merge into `casestmt/tcasestmt.nim` once testament has a VM target

# case stmt with string slice-lists
static:
  proc p(str: string): int =
    case str
    of "a": 1
    of "b".."e", "l".."o", "p": 2 # slice-list with gap
    of "f".."k": 3
    of "q".."z": 4
    #of "9".."1": 5 # empty range; rejected by sem
    else: 6

  doAssert p("a") == 1
  doAssert p("d") == 2
  doAssert p("h") == 3
  doAssert p("p") == 2
  doAssert p("z") == 4
  doAssert p("z1") == 6
  doAssert p("2") == 6
  doAssert p("other") == 6

# case stmt with float slice-lists
static:
  proc p(f: float): int =
    case f
    of 5.0, -2.0 .. 1.2, 6.0: 0 # slice-list with gaps
    of 1.5: 1
    of 20.0 .. 32.0: 2
    else: 3

  # direct matches
  doAssert p(5.0) == 0
  doAssert p(-2.0) == 0
  doAssert p(1.2) == 0
  doAssert p(6.0) == 0
  doAssert p(1.5) == 1
  doAssert p(20.0) == 2
  doAssert p(32.0) == 2

  # in ranges
  doAssert p(0.1) == 0
  doAssert p(-1.0) == 0
  doAssert p(1.1) == 0
  doAssert p(1.6) == 3
  doAssert p(25.0) == 2