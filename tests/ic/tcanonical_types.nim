discard """
  description: '''
    Regression test for compiler crash due to missing hook attachements. The
    test is contrived and depends on compiler implementation details.
  '''
"""

proc f1(x: ref int): ref int =
  # the assignment requires the =copy hook. `f1` is processed by hook lifting
  # first, so the lifetime hooks are bound to the result variable's ``ref int``
  # instance
  result = x

proc f2(x: ref int) =
  discard

# use `f2` first so that its ``ref int`` instance is registered with the
# environment
f2(nil)
# then use `f1`. It's ``ref int`` instance is mapped to that of `f2`, and
# since `f2`'s ``ref int`` *instance* has no hook attached, missing canonical
# type mappings result in a crash (as no hook can be found)
discard f1(nil)
