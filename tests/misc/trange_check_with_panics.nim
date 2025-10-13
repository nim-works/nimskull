discard """
  description: '''
    Ensure that ``system.rangeCheck`` has no side-effects when panics are
    enabled
  '''
  targets: native
  action: compile
  matrix: "--panics:on"
"""

proc f() {.noSideEffect.} =
  # must compile
  rangeCheck(true)
