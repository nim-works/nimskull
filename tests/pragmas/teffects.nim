discard """
  description: "Tests for the `.effects.` pragma"
  action: compile
  nimout: '''
Hint: ref IOError
 [rsemEffectsListingHint]
Hint: ref IOError
ref ValueError
 [rsemEffectsListingHint]'''
"""

proc p(cond: bool) =
  if cond:
    raise IOError.newException("")
    {.effects.}
  else:
    raise ValueError.newException("")

  {.effects.}

p(false)