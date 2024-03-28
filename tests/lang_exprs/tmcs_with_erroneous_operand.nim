discard """
  description: '''
    Ensure that using the method-call syntax doesn't result in an error when
    the LHS cannot be typed, but an eligible template/macro with an untyped
    parameter exists.
  '''
  action: compile
"""

template f(x: untyped) =
  discard

template f(x, y: untyped) =
  discard

template `f=`(x, y: untyped) =
  discard

# must not result in an "undeclared identifier" error
nonexistentName.f
nonexistentName.f(1)
nonexistentName.f = 1
