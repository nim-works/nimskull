discard """
  description: '''
    This is a regression test for a semantic analysis issue that was caused by
    a direct type modification
  '''
  action: compile
  target: native
"""

proc p1() =
  var x: seq[(bool, bool)]
  for _, a in x.mpairs: # doesn't mutate the type
    a[0] = true # works

proc p2() =
  var x: seq[(bool, bool)]
  for _, (_, _) in x.mpairs:
    # mutates the type of the instantiated ``mpairs`` iterator (turns the
    # return type from ``(int, var (bool, bool))`` into ``(int, (bool, bool))``
    discard

proc p3() =
  var x: seq[(bool, bool)]
  for _, a in x.mpairs:
    a[0] = true # fails to compile: ``a[0]`` cannot be assigned to