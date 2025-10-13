discard """
  description: '''
    Ensure that non-overloaded symbols used as the method in the method-call
    syntax are closed symbols
  '''
  action: reject
  knownIssue: '''
    Non-overloaded symbols used as the callee with the method call syntax
    are always open
  '''
"""

proc p(x: int) =
  discard

template test(x: string) =
  # `p` is not overloaded at this point, and it's thus a closed symbol
  x.p()

# the overload with which the call would work:
proc p(x: string) =
  discard

# the symbol of the callee is bound early and is closed, so the correct
# overload cannot be picked, meaning that an error ensues
test("")
