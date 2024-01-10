discard """
  outputsub: "Error: unhandled exception:  [ValueError]"
  exitcode: "1"
  knownIssue.js: "the 'uncaught exception' message doesn't match"
"""
try:
  raise newException(ValueError, "")
except OverflowDefect:
  echo("Error caught")
