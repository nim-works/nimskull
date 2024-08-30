discard """
  description: '''
    Using user-defined typeclass in argument type works differently compared
    to the explicitly written type alternatrives.
  '''
  errormsg: "type mismatch: got <float, int>"
"""


block:
  ## When type alternative is used for each argument they will be
  ## constained independenyly of each other.
  proc separateTypeAlts(a: int or float; b: int or float) = discard
  separateTypeAlts(float(2.0), int(2))

block:
  ## If typeclass is used the `distinct` keyword should be added if
  ## arguments must be constrained separately.
  type Typeclass = int or float

  block:
    proc aliasedTypeAlts(a: Typeclass; b: distinct Typeclass) = discard
    aliasedTypeAlts(float(2.0), int(2))

  block:
    ## `distinct` does not have to be added to each argument separately,
    ## `a, b: Type` can also be used.
    proc aliasedTypeAlts(a, b: distinct Typeclass) = discard
    aliasedTypeAlts(float(2.0), int(2))

block:
  type Typeclass = int or float

  ## If typeclass is not specified with a `distinct` keyword, all
  ## arguments will be constrained to a specific type at once. That is -
  ## once `a` is bound to `int` then `b` must also be an integer.
  proc aliasedTypeAlts(a: Typeclass; b: Typeclass) = discard
  aliasedTypeAlts(float(2.0), int(2))


