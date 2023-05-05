discard """
  action: compile
  knownIssue: '''
    A type alias doesn't produce a separate procedure instantiation, and the
    instantiation is remembered with the alias type, not the skipped type
  '''
"""

# note: `Alias` is currently not a ``tyAlias``, but a separate ``tyString``
# instance
type Alias = string

proc test[T](): T =
  discard

static:
  # swapping both lines would make the "Alias" assertion fail instead of the
  # "string" one.
  doAssert($typeof(test[Alias]()) == "Alias") # works
  doAssert($typeof(test[string]()) == "string") # fails; reports "Alias"